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
Chd|  I22SUBVOL                     source/interfaces/int22/i22subvol.F
Chd|-- called by -----------
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE I22SUBVOL(X    ,NIN     ,ITASK  ,IPARI  ,ITAB     , 
     .                     IXS  ,IXTG    ,V      ,IPARG  ,ELBUF_TAB,
     .                     W    ,IGRSH3N )
C============================================================================      
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandonned, and is not an official option.
C
C CThis subroutine computes coordinates of intersection points and faces area
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE I22BUFBRIC_MOD
      USE I22TRI_MOD
      USE I22EDGE_MOD
      USE ELBUFDEF_MOD
      USE GROUPDEF_MOD
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
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "subvolumes.inc"
#include      "i22edges.inc"
#include      "inter22.inc"
C-----------------------------------------------
      INTERFACE
        FUNCTION I22AERA(NPTS,P, C)
          INTEGER, INTENT(IN)          :: NPTS
          my_real, INTENT(IN)          :: P(3,NPTS)     
          my_real, INTENT(INOUT)       :: C(3)  
          my_real :: I22AERA(3)
        END FUNCTION
      END INTERFACE
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER :: NIN, ITASK,IbugANIM22, IXS(NIXS,*), IPARI(48:50),
     .           ITAB(*),IXTG(NIXTG,*), IPARG(NPARG,*)
      my_real, intent(inout), TARGET ::
     .                       X(3,*),V(3,*),W(3,*)
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
!
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K,N, IAD(7),  NBCUT, ICUT(8,7), IB, ICELL, NTRUS,INOD,
     .        IEDG(6) , KK , II  , IAD0, IDSPRI, SECID, Fa(0:6),
     .        ID, ICODE, IDBLE,M, IE, NBF, NBL,NNOD, MNOD, NINTPOINT, M_SUM,NUMPT,IGR
      my_real
     .         CUTPOINT(3), CUTCOOR, Vmin, Vmax, VOL, Vol1, Vol2
      CHARACTER*14 SECTYPE, SECTYPE2
      my_real :: vF(1:3,0:6)
!      my_real,DIMENSION(:)  , POINTER :: pFACE
      TYPE(POLYFACE_ENTITY),dimension(:),pointer      :: pFACE
      my_real               , POINTER :: pFACE0  
!      INTEGER,DIMENSION(:)  , POINTER :: pNNodCell
!      INTEGER,DIMENSION(:)  , POINTER :: pNPointCell      
!      INTEGER,DIMENSION(:)  , POINTER :: pWhichCellNod
      my_real,DIMENSION(:,:), POINTER :: pCUT    !=> BRICK_ENTITY%CUTCOOR(3,2)
!      my_real,DIMENSION(:)  , POINTER :: pSUBVOL !=> BRICK_ENTITY%Vnew
      TYPE(POLY_ENTITY), DIMENSION(:), POINTER :: pSUBVOL,pNNodCell,pNPointCell
      TYPE(NODE_ENTITY), DIMENSION(:), POINTER :: pWhichCellNod
      my_real,DIMENSION(:)  , POINTER :: pVOL    !=> BRICK_ENTITY%VOL(2)
      my_real,DIMENSION(:)  , POINTER :: a1,a2,a3,a4,a5,a6
      my_real                         :: wa1(3),wa2(3),wa3(3),wa4(3),wa5(3),wa6(3)
      my_real                         :: wn1(3),wn2(3),wn3(3),wn4(3),wn5(3),wn6(3)
      my_real                         :: wf1(3),wf2(3),wf3(3),wf4(3),wf5(3),wf6(3) , wf0(3)
      my_real,DIMENSION(:)  , POINTER :: n1,n2,n3,n4 ,n5,n6,n7,n8, nn
      my_real :: C(1:3,0:6), SCUT, Ctmp(3), ValTMP(3)
      my_real :: diag1(3), diag2(3), UncutVOL, Z(3,6), VolRatioMin
      
      integer :: pNNodCell_bak(2), EdgNod1, EdgNod2
      INTEGER :: Cod1, Cod2, Cod1_bis, Cod2_bis
      LOGICAL :: bAMBIGOUS, IsSH3N, bTWICE
      INTEGER :: INodTAG(8), IEtab(8),IEnod(1:4), IPCUT, ICRIT,ISHELL,NSH3N, NTRIA(1:3),NCELL
      my_real :: PCut_vect(1:3,4), IEvect(1:3,8), CRITERIA(2), pt(3,3), VOLratio,NORM,dist,tmp(3),scut1,scut2,beta
      LOGICAL :: bCOMPL
      INTEGER :: nFACE, IPOS, iCUTe(2,12), iPOSe(12), tagEDG, iCOMPL
      INTEGER :: EdgePos(1:16,2), EdgeList(1:16) !12<2**4
      INTEGER :: ITAG(1:8),NFL,NEL,NEL_B,NG,IDLOC
      LOGICAL :: lFOUND
      CHARACTER*14 :: tmp_SECTYPE(8)
      INTEGER      :: tmp_SecID_CELL(8),tmp_NumNOD(8),tmp_NumPOINT(8),NTAG

      TYPE(POLY_ENTITY)tmpPOLY(8)  
      TYPE(CUT_PLANE)tmpCUT(8) 
C----------------------------------------------------------------

!A FAIRE : SI PAS DE BRIQUE OU PLUS DE BRIQUE, METTRE QUAND MEME NOEUDS A 0.
        !----------------------------------------------------------------
        !debug:
        if (IBUG22_subvol==1.AND.itask==0)then
           print *,""
           print *, "================================="
           print *, "========    SUBVOL    ==========="
           print *, "================================="           
        endif

      VolRatioMin =  EM10  

      IGR   = IPARI(49)  ! sh3n group identifier
      NEL   = IPARI(50)  ! sh3n group elements
      II    = 1  ! attention, first sh3n element of the group

      NBF   = 1+ITASK*NB/NTHREAD
      NBL   = (ITASK+1)*NB/NTHREAD
      
      DO IB=NBF,NBL !1,NB
      
        DO K=1,6
        BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(K)%Center(1) = ZERO
        BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(K)%Center(2) = ZERO
        BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(K)%Center(3) = ZERO
        ENDDO

        ICODE                  = BRICK_LIST(NIN,IB)%ICODE
        IDBLE                  = BRICK_LIST(NIN,IB)%IDBLE 
        bTWICE                 = .FALSE.
        IF(ICODE==IDBLE)bTWICE = .TRUE.       
      
        SECID                  =  0
        ID                     =  BRICK_LIST(NIN,IB)%ID
        pSUBVOL(1:9)           => BRICK_LIST(NIN,IB)%POLY(1:9)!%Vnew
        pSUBVOL(1:9)%Vnew      =  ZERO
        pNNodCell(1:9)         => BRICK_LIST(NIN,IB)%POLY(1:9)!%NumNOD
        pNNodCell(1:9)%NumNOD  =  0
        pNPointCell(1:9)       => BRICK_LIST(NIN,IB)%POLY(1:9)!%NumPOINT 
        pNPointCell(1:9)%NumPOINT        =  0  
        !pListNodID             => BRICK_LIST(NIN,IB)%POLY(1:9)%ListNodID(1:8)
       
        BRICK_LIST(NIN,IB)%POLY(1:9)%ListNodID(1) = 0
        BRICK_LIST(NIN,IB)%POLY(1:9)%ListNodID(2) = 0
        BRICK_LIST(NIN,IB)%POLY(1:9)%ListNodID(3) = 0
        BRICK_LIST(NIN,IB)%POLY(1:9)%ListNodID(4) = 0
        BRICK_LIST(NIN,IB)%POLY(1:9)%ListNodID(5) = 0
        BRICK_LIST(NIN,IB)%POLY(1:9)%ListNodID(6) = 0
        BRICK_LIST(NIN,IB)%POLY(1:9)%ListNodID(7) = 0
        BRICK_LIST(NIN,IB)%POLY(1:9)%ListNodID(8) = 0
                  
        BRICK_LIST(NIN,IB)%POLY(1:9)%FACE0%Surf   = ZERO
        DO K=1,6
        BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(K)%Surf = ZERO
        ENDDO
        
        NINTPOINT              = 0
        M_SUM                  = 0 
        pWhichCellNod(1:8)     => BRICK_LIST(NIN,IB)%NODE(1:8)!%WhichCell
        pWhichCellNod(1:8)%WhichCell     = 0   
        EdgeList(1:16)         = 0    
        EdgePos(1:16,1:2)      = 0
        
        DO K=1,6
        BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(K)%Vel(1) = ZERO
        BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(K)%Vel(2) = ZERO
        BRICK_LIST(NIN,IB)%POLY(1:9)%FACE(K)%Vel(3) = ZERO
        ENDDO
        BRICK_LIST(NIN,IB)%PCUT(1:9)%Vel(1)   = ZERO
        BRICK_LIST(NIN,IB)%PCUT(1:9)%Vel(2)   = ZERO
        BRICK_LIST(NIN,IB)%PCUT(1:9)%Vel(3)   = ZERO
        
        !--------------------------------------------------------------------------------!
        !  1. CHECK FIRST IF AMBIGOUS COMBINATION                                        !
        !--------------------------------------------------------------------------------!
        !bAMBIGOUS == .TRUE. => ambigous combination detected
        ! (code1_bis, code2_bis) is then the alternative combination
        IF(BRICK_LIST(NIN,IB)%NBCUT == 2)THEN
          SECTYPE = BRICK_LIST(NIN,IB)%SECTYPE(1)
          Cod1    = IABS(BRICK_LIST(NIN,IB)%SecID_CELL(1))
          bAMBIGOUS = .FALSE.          
          IF(SECTYPE(1:5)=='PENTA')THEN
            SECTYPE = BRICK_LIST(NIN,IB)%SECTYPE(2)
            Cod2    = IABS(BRICK_LIST(NIN,IB)%SecID_CELL(2))
            IF(SECTYPE(1:5)=='PENTA')THEN
              IF     (Cod1==10 .AND. Cod2==17 )THEN  
                bAMBIGOUS = .TRUE. 
                Cod1_bis  =  12  
                Cod2_bis  =  19                         
              ELSEIF (Cod1==11 .AND. Cod2==14 )THEN
                bAMBIGOUS = .TRUE. 
                Cod1_bis  =  15
                Cod2_bis  =  18                               
              ELSEIF (Cod1==12 .AND. Cod2==19 )THEN
                bAMBIGOUS = .TRUE. 
                Cod1_bis  =  10  
                Cod2_bis  =  17                               
              ELSEIF (Cod1==13 .AND. Cod2==16 )THEN
                bAMBIGOUS = .TRUE.
                Cod1_bis  =  09 
                Cod2_bis  =  20                                
              ELSEIF (Cod1==15 .AND. Cod2==18 )THEN
                bAMBIGOUS = .TRUE.  
                Cod1_bis  =  11  
                Cod2_bis  =  14                              
              ELSEIF (Cod1== 09 .AND. Cod2==20 )THEN  
                bAMBIGOUS = .TRUE. 
                Cod1_bis  =  13  
                Cod2_bis  =  16 
              ENDIF                                                                                    
            ENDIF      
          ENDIF
          !CHECK SURFACE NORMAL AND SOLVE THE AMBIGUITY
          IF(.NOT.bAMBIGOUS)THEN
            !print *, "not ambigous"
          ELSE
            !print *, "ambigous=", Cod1,Cod2
            !----------------------------------------------------------------
            !RETRIEVING LAGRANGIAN FACES & CUT PLANE NORMAL
            !----------------------------------------------------------------            
            SECID            = Cod1
            IAD(1:4)         = (/((IB-1)*12+IABS(Gcorner(K,SECID)),K=1,4)/)
            IAD(5)           = IAD(1)
            ICUT(1:2,1:4)    = 1
            DO K=1,4 
              IF(EDGE_LIST(NIN,IAD(K))%NBCUT > 1 .AND. Gcorner(K,SECID) > 0)ICUT(1,K) = 2   !should be 1 but formulation might be updated later, keep general form using edge orientation (ICUT=2 possible).
CC              print *,"---1", IAD(K),ICUT(1,K), EDGE_LIST(NIN,IAD(K))%CUTSHELL(ICUT(1,K))              
              IEtab(K)      = EDGE_LIST(NIN,IAD(K))%CUTSHELL(ICUT(1,K))
              CUTCOOR       = EDGE_LIST(NIN,IAD(K))%CUTCOOR(ICUT(1,K))             ! gerer les intersections doubles cf commentaire plus haut
              CUTPOINT(1:3) = X(1:3, EDGE_LIST(NIN,IAD(K))%NODE(1) ) + CUTCOOR * (EDGE_LIST(NIN,IAD(K))%VECTOR(1:3))            
              EDGE_LIST(NIN,IAD(K))%CUTPOINT(1:3,ICUT(1,1)) = CUTPOINT(1:3)
            END DO !NEXT K
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,ICUT(1,1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,ICUT(1,2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,ICUT(1,3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,ICUT(1,4))
            vF(:,0) = I22AERA(4,(/a1,a2,a3,a4/),C(1:3,   0 )) !Cut surface normal vector (not unitary : 2S factor) 
            ! NE peut pas valoir zero si on borne les intersections a CUTCOOR = EM06 et ONE-EM06 dans i22intersect.F  (0 et 1.00    tant impossible => pas de PENTA degenere avec Scut=0.000, et pas de division par 0)
            PCut_vect(1:3,1) = vF(:,0) / SQRT(SUM(vF(:,0)*vF(:,0)))             
            !----------------------------------------------------------------
            SECID            = Cod2
            IAD(1:4)         = (/((IB-1)*12+IABS(Gcorner(K,SECID)),K=1,4)/)
            IAD(5)           = IAD(1)
            DO K=1,4 
              IF(EDGE_LIST(NIN,IAD(K))%NBCUT > 1 .AND. Gcorner(K,SECID) > 0)ICUT(2,K) = 2   !should be 1 but formulation might be updated later, keep general form using edge orientation (ICUT=2 possible).
CC              print *,"---2", IAD(K),ICUT(2,K), EDGE_LIST(NIN,IAD(K))%CUTSHELL(ICUT(2,K))
              IEtab(4+K)    = EDGE_LIST(NIN,IAD(K))%CUTSHELL(ICUT(2,K))
              CUTCOOR       = EDGE_LIST(NIN,IAD(K))%CUTCOOR(ICUT(2,K))             ! gerer les intersections doubles cf commentaire plus haut
              CUTPOINT(1:3) = X(1:3, EDGE_LIST(NIN,IAD(K))%NODE(1) ) + CUTCOOR * (EDGE_LIST(NIN,IAD(K))%VECTOR(1:3))            
              EDGE_LIST(NIN,IAD(K))%CUTPOINT(1:3,ICUT(2,1)) = CUTPOINT(1:3)              
            END DO !NEXT K
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,ICUT(2,1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,ICUT(2,2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,ICUT(2,3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,ICUT(2,4))
            vF(:,0) = I22AERA(4,(/a1,a2,a3,a4/),C(1:3,   0 )) !Cut surface normal vector (not unitary : 2S factor) 
            ! NE peut pas valoir zero si on borne les intersections a CUTCOOR = EM06 et ONE-EM06 dans i22intersect.F  (0 et 1.00    tant impossible => pas de PENTA degenere avec Scut=0.000, et pas de division par 0)
            PCut_vect(1:3,2) = vF(:,0) / SQRT(SUM(vF(:,0)*vF(:,0))) 
            !----------------------------------------------------------------            
            SECID            = Cod1_bis
            IAD(1:4)         = (/((IB-1)*12+IABS(Gcorner(K,SECID)),K=1,4)/)
            IAD(5)           = IAD(1)
            ICUT(1,1:4)        = 1
            DO K=1,4 
              IF(EDGE_LIST(NIN,IAD(K))%NBCUT > 1 .AND. Gcorner(K,SECID) > 0)ICUT(1,K) = 2   !should be 1 but formulation might be updated later, keep general form using edge orientation (ICUT=2 possible).
              !IEtab(4+K)    = EDGE_LIST(NIN,IAD(K))%CUTSHELL(ICUT(1,K))
              CUTCOOR       = EDGE_LIST(NIN,IAD(K))%CUTCOOR(ICUT(1,K))             ! gerer les intersections doubles cf commentaire plus haut
              CUTPOINT(1:3) = X(1:3, EDGE_LIST(NIN,IAD(K))%NODE(1) ) + CUTCOOR * (EDGE_LIST(NIN,IAD(K))%VECTOR(1:3))            
              EDGE_LIST(NIN,IAD(K))%CUTPOINT(1:3,ICUT(1,1)) = CUTPOINT(1:3)              
            END DO !NEXT K
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,ICUT(1,1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,ICUT(1,2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,ICUT(1,3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,ICUT(1,4))
            vF(:,0) = I22AERA(4,(/a1,a2,a3,a4/),C(1:3,   0 )) !Cut surface normal vector (not unitary : 2S factor) 
            ! NE peut pas valoir zero si on borne les intersections a CUTCOOR = EM06 et ONE-EM06 dans i22intersect.F  (0 et 1.00    tant impossible => pas de PENTA degenere avec Scut=0.000, et pas de division par 0)
            PCut_vect(1:3,3) = vF(:,0) / SQRT(SUM(vF(:,0)*vF(:,0)))  
            !----------------------------------------------------------------            
            SECID            = Cod2_bis
            IAD(1:4)         = (/((IB-1)*12+IABS(Gcorner(K,SECID)),K=1,4)/)
            IAD(5)           = IAD(1)
            ICUT(2,1:4)        = 1
            DO K=1,4 
              IF(EDGE_LIST(NIN,IAD(K))%NBCUT > 1 .AND. Gcorner(K,SECID) > 0)ICUT(2,K) = 2   !should be 1 but formulation might be updated later, keep general form using edge orientation (ICUT=2 possible).
              !IEtab(4+K)    = EDGE_LIST(NIN,IAD(K))%CUTSHELL(ICUT(2,K))
              CUTCOOR       = EDGE_LIST(NIN,IAD(K))%CUTCOOR(ICUT(2,K))             ! gerer les intersections doubles cf commentaire plus haut
              CUTPOINT(1:3) = X(1:3, EDGE_LIST(NIN,IAD(K))%NODE(1) ) + CUTCOOR * (EDGE_LIST(NIN,IAD(K))%VECTOR(1:3))            
              EDGE_LIST(NIN,IAD(K))%CUTPOINT(1:3,ICUT(2,1)) = CUTPOINT(1:3)              
            END DO !NEXT K
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,ICUT(2,1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,ICUT(2,2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,ICUT(2,3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,ICUT(2,4))
            vF(:,0) = I22AERA(4,(/a1,a2,a3,a4/),C(1:3,   0 )) !Cut surface normal vector (not unitary : 2S factor)
            ! NE peut pas valoir zero si on borne les intersections a CUTCOOR = EM06 et ONE-EM06 dans i22intersect.F  (0 et 1.00    tant impossible => pas de PENTA degenere avec Scut=0.000, et pas de division par 0) 
            PCut_vect(1:3,4) = vF(:,0) / SQRT(SUM(vF(:,0)*vF(:,0)))                                        
            !----------------------------------------------------------------
            !COMPUTING NORMALS AT LAGRANGIAN FACES
            !----------------------------------------------------------------            
            DO I=1,8
              IE = IEtab(I)
              IEnod(3:4)=IRECT_L(3:4,IE)
              IF(IEnod(3)==IEnod(4))THEN 
                IsSH3N=.true.
                Diag1(1:3)    = IRECT_L((/5,9,13/),IE) - IRECT_L((/6,10,14/),IE)
                Diag2(1:3)    = IRECT_L((/5,9,13/),IE) - IRECT_L((/7,11,15/),IE)
                IEvect(1,I)   = + Diag1(2)*Diag2(3) - diag1(3)*Diag2(2)
                IEvect(2,I)   = + Diag1(3)*Diag2(1) - diag1(1)*Diag2(3)
                IEvect(3,I)   = + Diag1(1)*Diag2(2) - diag1(2)*Diag2(1)
                IEvect(1:3,I) = HALF * IEvect(1:3,I) / SQRT(SUM( IEvect(1:3,I) * IEvect(1:3,I) ))
              ELSE
                IsSH3N=.false.
                Diag1(1:3)    = IRECT_L((/5, 9,13/),IE) - IRECT_L((/7,11,15/),IE)
                Diag2(1:3)    = IRECT_L((/6,10,14/),IE) - IRECT_L((/8,12,16/),IE) 
                IEvect(1,I)   =  + Diag1(2)*Diag2(3) - diag1(3)*Diag2(2)
                IEvect(2,I)   =  + Diag1(3)*Diag2(1) - diag1(1)*Diag2(3)
                IEvect(3,I)   =  + Diag1(1)*Diag2(2) - diag1(2)*Diag2(1)
                IEvect(1:3,I) = HALF * IEvect(1:3,I) / SQRT(SUM( IEvect(1:3,I) * IEvect(1:3,I) ))                 
              ENDIF              
            ENDDO
            !----------------------------------------------------------------            
            !COMPUTING DETERMINATION CRITERIA
            !----------------------------------------------------------------            
            CRITERIA(:) = ZERO
            DO ICRIT = 1,2
              DO IPCUT = 1,2
                DO J = 1,4
                  CRITERIA(ICRIT) = CRITERIA(ICRIT) + ABS( SUM(IEvect(1:3,(IPCUT-1)*4+J)*PCut_vect( 1:3 , (ICRIT-1)*2 + IPCUT)  ) )
                ENDDO! J
              ENDDO! PCUT
            ENDDO! ICRIT
            !----------------------------------------------------------------
            !DETERMINING THE CORRECT COMBINATION
            !----------------------------------------------------------------            
            IF    (CRITERIA(1) > CRITERIA(2))THEN
              !keep it             
            ELSEIF(CRITERIA(2) > CRITERIA(1))THEN
              !change it
              BRICK_LIST(NIN,IB)%SecID_CELL(1:2) = (/ Cod1_bis, Cod2_bis /)
              BRICK_LIST(NIN,IB)%SECTYPE(1)      = StrCode(Cod1_bis)
              BRICK_LIST(NIN,IB)%SECTYPE(2)      = StrCode(Cod2_bis) 
CC              print *, "CHANGING RETAINED COMBINATION TO :",StrCode(cod1_bis),StrCode(cod2_bis)
            ELSE
CC              print *, "inter22 : ambigus intersection not solved" ;        
            ENDIF 
            !----------------------------------------------------------------                     
          ENDIF!(bAMBIGOUS)
        ENDIF!(BRICK_LIST(NIN,IB)%NBCUT == 2)


        !--------------------------------------------------------------------------------!
        ! 2.   COMPUTE POLYHEDRON VOLUME AND FACES                                       !
        !--------------------------------------------------------------------------------!
        NCELL            = BRICK_LIST(NIN,IB)%NBCUT
        ICUTe(1:2,1:12)  = 0
        
        !normilized volume 
        UNCUTVOL = ONE
        DO NG=1,NGROUP
          NEL_B=IPARG(2,NG)
          NFL  =IPARG(3,NG)
          IF( ID>NFL.AND. ID<=NFL+NEL_B)THEN
            IDLOC = ID - NFL 
            UNCUTVOL = ELBUF_TAB(NG)%GBUF%VOL(IDLOC)
            EXIT
          ENDIF
        ENDDO!next ng
        
        ITAG(1:8) = 0
        
        DO ICELL=1, NCELL
          M            = 0
          NTRUS        = 0
          NNOD         = 0
          MNOD         = 0
          SECTYPE      = BRICK_LIST(NIN,IB)%SECTYPE(ICELL)
          SECID        = BRICK_LIST(NIN,IB)%SecID_Cell(ICELL)
          bCOMPL       = .FALSE.
          IF(SECID<0)THEN
            bCOMPL = .TRUE.
            SECID  = - SECID
          ENDIF
          INodTAG(1:8) = 0          
          IF(SECTYPE(1:5)=='TETRA')THEN
            NTRUS= C_TETRA
            M    = M_TETRA
            NNOD = N_TETRA+C_TETRA
            MNOD = N_TETRA
          ELSEIF(SECTYPE(1:5)=='PENTA')THEN
            NTRUS= C_PENTA
            M    = M_PENTA
            NNOD = N_PENTA+C_PENTA 
            MNOD = N_PENTA                       
          ELSEIF(SECTYPE(1:5)=='POLY3')THEN
            NTRUS= C_POLY3
            M    = M_POLY3
            NNOD = N_POLY3+C_POLY3 
            MNOD = N_POLY3                       
          ELSEIF(SECTYPE(1:5)=='HEXAE')THEN
            NTRUS= C_HEXAE
            M    = M_HEXAE
            NNOD = N_HEXAE+C_HEXAE  
            MNOD = N_HEXAE                      
          ELSEIF(SECTYPE(1:6)=='POLY4 ')THEN
            NTRUS= C_POLY4
            M    = M_POLY4
            NNOD = N_POLY4+C_POLY4 
            MNOD = N_POLY4     
          ELSEIF(SECTYPE(1:6)=='POLY4A')THEN
            NTRUS= C_POLY4A
            M    = M_POLY4A
            NNOD = N_POLY4A+C_POLY4A 
            MNOD = N_POLY4A     
          ELSEIF(SECTYPE(1:6)=='POLY4B')THEN
            NTRUS= C_POLY4B
            M    = M_POLY4B
            NNOD = N_POLY4B+C_POLY4B 
            MNOD = N_POLY4B                             
          ELSEIF(SECTYPE(1:5)=='POLYC')THEN
            NTRUS= C_POLYC
            M    = M_POLYC
            NNOD = N_POLYC+C_POLYC
            MNOD = N_POLYc                                  
          END IF
          M_SUM     = M_SUM + M
          NINTPOINT = NINTPOINT + NTRUS
          IAD(1:NTRUS) = (/((IB-1)*12+IABS(Gcorner(K,SECID)),K=1,NTRUS)/)
          IAD(NTRUS+1) = IAD(1)
          DO K=1,NTRUS
            EdgeList(IABS(Gcorner(K,SECID))) = EdgeList(IABS(Gcorner(K,SECID))) + 1
          ENDDO

          !---------------------------------------------------------------- 
          !CALCUL DES COORDONNEES DES POINTS D'INTERSECTION
          !---------------------------------------------------------------- 
          !if twice the same intersections, the 2nd one  is the biggest one choose farest intersection points 
          DO K=1,NTRUS  
             tagEDG   =  Gcorner(K,SECID)
            IF(tagEDG<0)THEN
              !orientation negative : on prend le plus proche
              ipos = 1
              tagEDG = - tagEDG
            ELSE
            !on prend le plus eloigne
               IF(EDGE_LIST(NIN,IAD(K))%NBCUT==1)THEN
                ipos = 1
               ELSE
                ipos = 2
               ENDIF
            ENDIF
            EdgePos(IABS(tagEdg), ipos)              = 1
            IF(iCUTe(ipos,tagEDG)==1)ipos          = MOD(ipos,2)+1 !deja pris par polyhedre precedent, on prend lautre            
            CUTCOOR                                  = EDGE_LIST(NIN,IAD(K))%CUTCOOR(ipos)
            CUTPOINT(1:3)  = X(1:3, EDGE_LIST(NIN,IAD(K))%NODE(1) ) + CUTCOOR * (EDGE_LIST(NIN,IAD(K))%VECTOR(1:3))
            IE                                       = EDGE_LIST(NIN,IAD(K))%CUTSHELL(ipos)
            EDGE_LIST(NIN,IAD(K))%CUTPOINT(1:3,ipos) = CUTPOINT(1:3)
            EDGE_LIST(NIN,IAD(K))%CUTVEL(1:3,ipos)   = IRECT_L(24:26,IE)
            ICUTe(ipos, IABS(Gcorner(K,SECID)))      = 1 !on prendra le point dintersection suivant.
            iPOSe(K)                                 = ipos
          END DO !K
!          print *, "Gcorner = ", Gcorner(1:NTRUS,SECID)
!          print *, "iPOSe   = ", iPOSe(1:NTRUS)
  
          if( BRICK_LIST(NIN,IB)%ICODE==0.and.icell>=1)then
            print *, "error i22subvol."
            stop 221
          end if
          !----------------------------------------------------------------
          !   ANIMATION (positioning)
          !----------------------------------------------------------------
          NSH3N = GetNumTria(GetPolyhedraType(SECID))          
          IF(II<=NEL-NSH3N.AND.NSPMD<=1)THEN    !II=IAD0=0 si pas de grshel, Sinon cela signifie qu'il reste encore des shel non utilise dans le groupe au moins 4 pour faire une section poly4.    

C            if (IBUG22_subvol==1)WRITE (*,FMT='(A20,I10)') 
C     .       "path intersection : ",IXS(11,ID)
#include "lockon.inc"
            !on positionne les shell
            !NSH3N = GetNumTria(GetPolyhedraType(SECID))
c              print *,"i22subvol : brickID=", IXS(11,BRICK_LIST(NIN,IB)%ID)
c              print *,"i22subvol : icode  ="  , BRICK_LIST(NIN,IB)%ICODE          
c              print *,"i22subvol : idble  ="  , BRICK_LIST(NIN,IB)%IDBLE              
c              print *,"i22subvol : bTWIC  ="  , bTWICE             
c              print *,"i22subvol : icell  ="  , icell
c              print *,"i22subvol : typ    ="  , BRICK_LIST(NIN,IB)%SECTYPE(ICELL) , BRICK_LIST(NIN,IB)%SECID_Cell(ICELL)            
        
            DO K=1,NSH3N
              Ntria(1:3) = Gtria(1:3,K,GetPolyhedraType(SECID))            
              X(1:3,IXTG(2,IGRSH3N(IGR)%ENTITY(II))) = EDGE_LIST(NIN,IAD(Ntria(1)) )%CUTPOINT(1:3,iPOSe(Ntria(1)) )
              X(1:3,IXTG(3,IGRSH3N(IGR)%ENTITY(II))) = EDGE_LIST(NIN,IAD(Ntria(2)) )%CUTPOINT(1:3,iPOSe(Ntria(2)) )  
              X(1:3,IXTG(4,IGRSH3N(IGR)%ENTITY(II))) = EDGE_LIST(NIN,IAD(Ntria(3)) )%CUTPOINT(1:3,iPOSe(Ntria(3)) )                
              V(1:3,IXTG(2,IGRSH3N(IGR)%ENTITY(II))) = EDGE_LIST(NIN,IAD(Ntria(1)) )%CUTVEL(1:3,iPOSe(Ntria(1)) )
              V(1:3,IXTG(3,IGRSH3N(IGR)%ENTITY(II))) = EDGE_LIST(NIN,IAD(Ntria(2)) )%CUTVEL(1:3,iPOSe(Ntria(2)) )  
              V(1:3,IXTG(4,IGRSH3N(IGR)%ENTITY(II))) = EDGE_LIST(NIN,IAD(Ntria(3)) )%CUTVEL(1:3,iPOSe(Ntria(3)) ) 
C              print *, "          : sh3n_id=", IXTG(NIXTG,IBUFSSG(II))                             

c              if (IBUG22_subvol==1) then
c                pt(1,:) = EDGE_LIST(NIN,IAD(Ntria(1)) )%CUTPOINT(1:3,iPOSe(Ntria(1)))
c                pt(2,:) = EDGE_LIST(NIN,IAD(Ntria(2)) )%CUTPOINT(1:3,iPOSe(Ntria(2)))
c                pt(3,:) = EDGE_LIST(NIN,IAD(Ntria(3)) )%CUTPOINT(1:3,iPOSe(Ntria(3)))
c                WRITE(*,FMT='(I12,A,F16.9,A,F16.9,A,F16.9,A)') IXTG(NIXTG,IBUFSSG(II)),":(",pt(1,1),",",pt(1,2),",",pt(1,3),")"
c                WRITE(*,FMT='(I12,A,F16.9,A,F16.9,A,F16.9,A)') IXTG(NIXTG,IBUFSSG(II)),":(",pt(2,1),",",pt(2,2),",",pt(2,3),")"
c                WRITE(*,FMT='(I12,A,F16.9,A,F16.9,A,F16.9,A)') IXTG(NIXTG,IBUFSSG(II)),":(",pt(3,1),",",pt(3,2),",",pt(3,3),")"                                
c             endif
C               print *,"             ntria =", K
C               print *,"             II    =", II

              II=II+1 ! next sh3n               
            END DO !next K 
C             if (IBUG22_subvol==1)WRITE (*,FMT='(A1)') " "
#include "lockoff.inc"
          END IF !IAD>0
          !----------------------------------------------------------------          

          !----------------------------------------------------------------
          !   SUBVOLUME AERAS & VOLUMES CALCULATION
          ! il faut d'abord calcul des aires vectorielles
          !----------------------------------------------------------------
          Fa(0)    = 0
         !OPTIM : SUPPRIMER ABS si graphe tous orientes en normales sortantes.

          IF(SECTYPE(1:5)=='TETRA')THEN
            nFACE = F_TETRA
            !intersection points
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            n1    => X(1:3,IXS(Gnode(1,SECID)+1,ID))             
            Fa(1:3) = Gface(1:3,SECID)
            vF(:,1) = I22AERA(3,(/n1,a2,a1/), C(1:3,Fa(1)))
            vF(:,2) = I22AERA(3,(/n1,a3,a2/), C(1:3,Fa(2)))
            vF(:,3) = I22AERA(3,(/n1,a1,a3/), C(1:3,Fa(3)))
            vF(:,0) = I22AERA(3,(/a1,a2,a3/), C(1:3,   0 ))
            VOL = SUM((/(vF(:,I)*C(:,Fa(I)),I=0,3)/))
            VOL = THIRD * ABS(VOL)
            VOLratio = VOL/uncutVOL
            IF(VOLratio <= VolRatioMin) THEN
              ITAG(ICELL)=1 
              NINTPOINT = NINTPOINT - NTRUS    
              !print *,"cleaning 1"       
              CYCLE
            ENDIF
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3) + a2 + a1 !put sum intersection point in icell 9 (will be used in sinit22_fvm.F to compute faceCenter for poly9
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3) + a3 + a2
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3) + a1 + a3                                   
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%Center(1:3) = C(1:3,Fa(1))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%Center(1:3) = C(1:3,Fa(2))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%Center(1:3) = C(1:3,Fa(3)) 
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%Center(1:3)       = C(1:3,   0 )                        
            SCUT = SQRT(SUM(vF(:,0)*vF(:,0)))
            pFACE(1:6) => BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)!%Surf
            pFACE0=> BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Surf
            pFACE(Fa(1))%Surf = SQRT(SUM(vF(:,1)*vF(:,1)))    !pointers indexes always start from 1
            pFACE(Fa(2))%Surf = SQRT(SUM(vF(:,2)*vF(:,2)))
            pFACE(Fa(3))%Surf = SQRT(SUM(vF(:,3)*vF(:,3)))
            pFACE0         = SCUT
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%SCUT   = SCUT                         !aire de la surface de coupure    
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%B(1:3) = C(1:3,0)                     !centroid de la surface de coupure (i.e. basis point)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%N(1:3) = vF(:,0)                      !Cut surface normal vector (not unitary : 2S factor)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%NP     = 3                            !polygon points (number)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,1)= a1                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,2)= a2                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,3)= a3                          !polygon points (coordinates)                                 
            pSUBVOL(ICELL)%Vnew =  VOL
            pNNodCell(ICELL)%NumNOD = MNOD !main(brick nodes)
            pNPointCell(ICELL)%NumPOINT= NNOD !main + intersection
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(1)= SUM( (/a1(1),a2(1),a3(1),n1(1)/) ) / FOUR
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(2)= SUM( (/a1(2),a2(2),a3(2),n1(2)/) ) / FOUR
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(3)= SUM( (/a1(3),a2(3),a3(3),n1(3)/) ) / FOUR
            BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:MNOD) = Gnode(1:MNOD,SECID) !IXS(Gnode(1,SECID)+1,ID)
            INodTAG(Gnode(1:MNOD,SECID)) = 1 !tag local brick nodes [1 to 8] if used by polyhedron
            pWhichCellNod(Gnode(1:MNOD,SECID))%WhichCell = ICELL
            !VELOCITY ON POLYHEDRA FACES
            a1 => EDGE_LIST(NIN,IAD(1))%CUTVEL(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTVEL(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTVEL(1:3,iPOSe(3)) 
            n1 => V(1:3,IXS(Gnode(1,SECID)+1,ID))  
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(1),ICELL) = THIRD* (n1(1:3) + a2(1:3) + a1(1:3))
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(2),ICELL) = THIRD* (n1(1:3) + a3(1:3) + a2(1:3))
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(3),ICELL) = THIRD* (n1(1:3) + a1(1:3) + a3(1:3))                        
            !BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Vel(1:3) = THIRD* (a1(1:3) + a2(1:3) + a3(1:3))
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%Vel(1:3) = THIRD* (a1(1:3) + a2(1:3) + a3(1:3)) 

          ELSEIF(SECTYPE(1:5)=='PENTA')THEN
            nFACE = F_PENTA     
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,iPOSe(4))
            n1 => X(1:3,IXS(Gnode(1,SECID)+1,ID))
            n2 => X(1:3,IXS(Gnode(2,SECID)+1,ID))
            Fa(1:4) = Gface(1:4,SECID)
            vF(:,1) = I22AERA(3,(/n1,a2,a1/),   C(1:3,Fa(1)))
            vF(:,2) = I22AERA(3,(/n2,a4,a3/),   C(1:3,Fa(2)))          
            vF(:,3) = I22AERA(4,(/n1,n2,a3,a2/),C(1:3,Fa(3)))
            vF(:,4) = I22AERA(4,(/n2,n1,a1,a4/),C(1:3,Fa(4)))
            vF(:,0) = I22AERA(4,(/a1,a2,a3,a4/),C(1:3,   0 ))
            VOL = SUM((/(vF(:,I)*C(1:3,Fa(I)),I=0,4)/))
            VOL = THIRD * ABS(VOL)
            VOLratio = VOL/uncutVOL
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3)+ a2 + a1 !put sum intersection point in icell 9 (will be used in sinit22_fvm.F to compute faceCenter for poly9
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3)+ a4 + a3
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3)+ a3 + a2             
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(4))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(4))%Center(1:3)+ a1 + a4              
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%Center(1:3) = C(1:3,Fa(1))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%Center(1:3) = C(1:3,Fa(2))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%Center(1:3) = C(1:3,Fa(3))                                    
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(4))%Center(1:3) = C(1:3,Fa(4))   
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%Center(1:3)       = C(1:3,   0 )                                       
            SCUT = SQRT(SUM(vF(:,0)*vF(:,0)))
            pFACE(1:6) => BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)!%Surf
            pFACE0=> BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Surf
            pFACE(Fa(1))%Surf = SQRT(SUM(vF(:,1)*vF(:,1)))
            pFACE(Fa(2))%Surf = SQRT(SUM(vF(:,2)*vF(:,2)))
            pFACE(Fa(3))%Surf = SQRT(SUM(vF(:,3)*vF(:,3)))
            pFACE(Fa(4))%Surf = SQRT(SUM(vF(:,4)*vF(:,4)))
            pFACE0         = SCUT
            IF(VOLratio <= VolRatioMin) THEN
              IF(SCUT / EXP(0.666*LOG(UNCUTVOL)) <=  EM03)THEN 
                ITAG(ICELL)=1 
                NINTPOINT = NINTPOINT - NTRUS  
                !print *,"cleaning 2"         
                CYCLE
              ENDIF
            ENDIF
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%SCUT   = SCUT                         !aire de la surface de coupure    
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%B(1:3) = C(1:3,0)                     !centroid de la surface de coupure (i.e. basis point)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%N(1:3) = vF(:,0)                      !Cut surface normal vector (not unitary : 2S factor)          
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%NP     = 4                            !polygon points (number)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,1)= a1                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,2)= a2                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,3)= a3                          !polygon points (coordinates)           
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,4)= a4                          !polygon points (coordinates)                     
            pSUBVOL(ICELL)%Vnew =  VOL
            pNNodCell(ICELL)%NumNOD = MNOD !main(brick nodes)
            pNPointCell(ICELL)%NumPOINT= NNOD    
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(1)= SUM( (/a1(1),a2(1),a3(1),a4(1),n1(1),n2(1)/) ) / SIX
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(2)= SUM( (/a1(2),a2(2),a3(2),a4(2),n1(2),n2(2)/) ) / SIX
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(3)= SUM( (/a1(3),a2(3),a3(3),a4(3),n1(3),n2(3)/) ) / SIX 
            BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:MNOD) = Gnode(1:MNOD,SECID) !IXS(Gnode(1,SECID)+1,ID)
            INodTAG(Gnode(1:MNOD,SECID)) = 1 !tag local brick nodes [1 to 8] if used by polyhedron  
            pWhichCellNod(Gnode(1:MNOD,SECID))%WhichCell = ICELL
            !VELOCITY ON POLYHEDRA FACES
            a1 => EDGE_LIST(NIN,IAD(1))%CUTVEL(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTVEL(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTVEL(1:3,iPOSe(3)) 
            a4 => EDGE_LIST(NIN,IAD(4))%CUTVEL(1:3,iPOSe(4))             
            n1 => V(1:3,IXS(Gnode(1,SECID)+1,ID))  
            n2 => V(1:3,IXS(Gnode(2,SECID)+1,ID))  
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(1),ICELL) = THIRD* (n1(1:3) + a2(1:3) + a1(1:3))
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(2),ICELL) = THIRD* (n2(1:3) + a4(1:3) + a3(1:3))
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(3),ICELL) = FOURTH* (n1(1:3) + n2(1:3) + a3(1:3) + a2(1:3))
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(4),ICELL) = FOURTH* (n2(1:3) + n1(1:3) + a3(1:3) + a2(1:3)) 
            !BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Vel(1:3) = FOURTH* (a1(1:3) + a2(1:3) + a3(1:3) + a4(1:3)) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%Vel(1:3) = FOURTH* (a1(1:3) + a2(1:3) + a3(1:3) + a4(1:3))                                              

          ELSEIF(SECTYPE(1:5)=='POLY3')THEN
            nFACE = F_POLY3  
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,iPOSe(4))
            a5 => EDGE_LIST(NIN,IAD(5))%CUTPOINT(1:3,iPOSe(5))
            n1 => X(1:3,IXS(Gnode(1,SECID)+1,ID))
            n2 => X(1:3,IXS(Gnode(2,SECID)+1,ID))
            n3 => X(1:3,IXS(Gnode(3,SECID)+1,ID))
            Fa(1:5) = Gface(1:5,SECID)
            vF(:,1) = I22AERA(5,(/n1,n2,n3,a3,a2/),C(1:3,Fa(1)))
            vF(:,2) = I22AERA(3,(/n3,a4,a3/),      C(1:3,Fa(2)))
            vF(:,3) = I22AERA(4,(/n3,n2,a5,a4/),   C(1:3,Fa(3)))
            vF(:,4) = I22AERA(4,(/n2,n1,a1,a5/),   C(1:3,Fa(4)))
            vF(:,5) = I22AERA(3,(/n1,a2,a1/),      C(1:3,Fa(5)))
            vF(:,0) = I22AERA(5,(/a1,a2,a3,a4,a5/),C(1:3,   0 ))
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3) + a3 + a2 !put sum intersection point in icell 9 (will be used in sinit22_fvm.F to compute faceCenter for poly9
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3) + a4 + a3
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3) + a5 + a4             
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(4))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(4))%Center(1:3) + a1 + a5              
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(5))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(5))%Center(1:3) + a2 + a1                         
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%Center(1:3) = C(1:3,Fa(1))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%Center(1:3) = C(1:3,Fa(2))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%Center(1:3) = C(1:3,Fa(3))                                    
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(4))%Center(1:3) = C(1:3,Fa(4))    
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(5))%Center(1:3) = C(1:3,Fa(5))   
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%Center(1:3)       = C(1:3,   0 )           
            SCUT = SQRT(SUM(vF(:,0)*vF(:,0)))  
            pFACE(1:6) => BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)!%Surf
            pFACE0=> BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Surf
            pFACE(Fa(1))%Surf = SQRT(SUM(vF(:,1)*vF(:,1)))
            pFACE(Fa(2))%Surf = SQRT(SUM(vF(:,2)*vF(:,2)))
            pFACE(Fa(3))%Surf = SQRT(SUM(vF(:,3)*vF(:,3)))
            pFACE(Fa(4))%Surf = SQRT(SUM(vF(:,4)*vF(:,4)))
            pFACE(Fa(5))%Surf = SQRT(SUM(vF(:,5)*vF(:,5)))
            pFACE0         = SCUT
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%SCUT   = SCUT                         !aire de la surface de coupure    
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%B(1:3) = C(1:3,0)                     !centroid de la surface de coupure (i.e. basis point)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%N(1:3) = vF(:,0)                      !Cut surface normal vector (not unitary : 2S factor)          
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%NP     = 5                            !polygon points (number)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,1)= a1                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,2)= a2                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,3)= a3                          !polygon points (coordinates)           
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,4)= a4                          !polygon points (coordinates)          
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,5)= a5                          !polygon points (coordinates)          
            VOL = SUM((/(vF(:,I)*C(1:3,Fa(I)),I=0,5)/))
            VOL = THIRD * ABS(VOL)
            pSUBVOL(ICELL)%Vnew =  VOL
            pNNodCell(ICELL)%NumNOD = MNOD !main(brick nodes) 
            pNPointCell(ICELL)%NumPOINT= NNOD                 
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(1)= SUM( (/a1(1),a2(1),a3(1),a4(1),a5(1),n1(1),n2(1),n3(1)/) ) / EIGHT
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(2)= SUM( (/a1(2),a2(2),a3(2),a4(2),a5(2),n1(2),n2(2),n3(2)/) ) / EIGHT
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(3)= SUM( (/a1(3),a2(3),a3(3),a4(3),a5(3),n1(3),n2(3),n3(3)/) ) / EIGHT    
            BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:MNOD) = Gnode(1:MNOD,SECID) !IXS(Gnode(1,SECID)+1,ID)
            INodTAG(Gnode(1:MNOD,SECID)) = 1 !tag local brick nodes [1 to 8] if used by polyhedron    
            pWhichCellNod(Gnode(1:MNOD,SECID))%WhichCell = ICELL  
            !VELOCITY ON POLYHEDRA FACES
            a1 => EDGE_LIST(NIN,IAD(1))%CUTVEL(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTVEL(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTVEL(1:3,iPOSe(3)) 
            a4 => EDGE_LIST(NIN,IAD(4))%CUTVEL(1:3,iPOSe(4))             
            a5 => EDGE_LIST(NIN,IAD(5))%CUTVEL(1:3,iPOSe(5))                         
            n1 => V(1:3,IXS(Gnode(1,SECID)+1,ID))  
            n2 => V(1:3,IXS(Gnode(2,SECID)+1,ID))  
            n3 => V(1:3,IXS(Gnode(3,SECID)+1,ID))             
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(1),ICELL) = ONE_FIFTH * (n1+n2+n3+a3+a2)
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(2),ICELL) = THIRD  * (n3+a4+a3)
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(3),ICELL) = FOURTH  * (n3+n2+a5+a4)
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(4),ICELL) = FOURTH  * (n2+n1+a1+a5) 
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(5),ICELL) = THIRD  * (n1+a2+a1)             
            !BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Vel(1:3) = ONE_FIFTH * (a1+a2+a3+a4+a5) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%Vel(1:3) = ONE_FIFTH * (a1+a2+a3+a4+a5)                                                                                

          ELSEIF(SECTYPE(1:5)=='HEXAE')THEN
            nFACE = F_HEXAE    
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,iPOSe(4))
            n1 => X(1:3,IXS(Gnode(1,SECID)+1,ID))
            n2 => X(1:3,IXS(Gnode(2,SECID)+1,ID))
            n3 => X(1:3,IXS(Gnode(3,SECID)+1,ID))
            n4 => X(1:3,IXS(Gnode(4,SECID)+1,ID))
            Fa(1:5) = Gface(1:5,SECID)
            vF(:,1) = I22AERA(4,(/n4,n3,n2,n1/), C(1:3,Fa(1)))
            vF(:,2) = I22AERA(4,(/n1,n2,a2,a1/), C(1:3,Fa(2)))
            vF(:,3) = I22AERA(4,(/n2,n3,a3,a2/), C(1:3,Fa(3)))
            vF(:,4) = I22AERA(4,(/n3,n4,a4,a3/), C(1:3,Fa(4)))
            vF(:,5) = I22AERA(4,(/n4,n1,a1,a4/), C(1:3,Fa(5)))
            vF(:,0) = I22AERA(4,(/a1,a2,a3,a4/), C(1:3,   0 ))
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3) + ZERO    !put sum intersection point in icell 9 (will be used in sinit22_fvm.F to compute faceCenter for poly9
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3) + a2 + a1
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3) + a3 + a2             
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(4))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(4))%Center(1:3) + a4 + a3              
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(5))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(5))%Center(1:3) + a1 + a4                                     
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%Center(1:3) = C(1:3,Fa(1))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%Center(1:3) = C(1:3,Fa(2))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%Center(1:3) = C(1:3,Fa(3))                                    
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(4))%Center(1:3) = C(1:3,Fa(4))    
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(5))%Center(1:3) = C(1:3,Fa(5)) 
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%Center(1:3)       = C(1:3,   0 )             
            SCUT = SQRT(SUM(vF(:,0)*vF(:,0)))   
            pFACE(1:6) => BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)!%Surf
            pFACE0=> BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Surf
            pFACE(Fa(1))%Surf = SQRT(SUM(vF(:,1)*vF(:,1)))
            pFACE(Fa(2))%Surf = SQRT(SUM(vF(:,2)*vF(:,2)))
            pFACE(Fa(3))%Surf = SQRT(SUM(vF(:,3)*vF(:,3)))
            pFACE(Fa(4))%Surf = SQRT(SUM(vF(:,4)*vF(:,4)))
            pFACE(Fa(5))%Surf = SQRT(SUM(vF(:,5)*vF(:,5)))
            pFACE0         = SCUT
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%SCUT   = SCUT                         !aire de la surface de coupure    
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%B(1:3) = C(1:3,0)                     !centroid de la surface de coupure (i.e. basis point)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%N(1:3) = vF(:,0)                      !Cut surface normal vector (not unitary : 2S factor)          
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%NP     = 4                            !polygon points (number)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,1)= a1                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,2)= a2                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,3)= a3                          !polygon points (coordinates)           
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,4)= a4                          !polygon points (coordinates)          
            VOL = SUM((/(vF(:,I)*C(1:3,Fa(I)),I=0,5)/))
            VOL = THIRD * ABS(VOL)
            pSUBVOL(ICELL)%Vnew =  VOL
            pNNodCell(ICELL)%NumNOD = MNOD !main(brick nodes)  
            pNPointCell(ICELL)%NumPOINT= NNOD                 
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(1)= SUM( (/a1(1),a2(1),a3(1),a4(1),n1(1),n2(1),n3(1),n4(1)/) ) / EIGHT
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(2)= SUM( (/a1(2),a2(2),a3(2),a4(2),n1(2),n2(2),n3(2),n4(2)/) ) / EIGHT
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(3)= SUM( (/a1(3),a2(3),a3(3),a4(3),n1(3),n2(3),n3(3),n4(3)/) ) / EIGHT  
            BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:MNOD) = Gnode(1:MNOD,SECID) !IXS(Gnode(1,SECID)+1,ID)
            INodTAG(Gnode(1:MNOD,SECID)) = 1 !tag local brick nodes [1 to 8] if used by polyhedron    
            pWhichCellNod(Gnode(1:MNOD,SECID))%WhichCell = ICELL  
            !VELOCITY ON POLYHEDRA FACES
            a1 => EDGE_LIST(NIN,IAD(1))%CUTVEL(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTVEL(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTVEL(1:3,iPOSe(3)) 
            a4 => EDGE_LIST(NIN,IAD(4))%CUTVEL(1:3,iPOSe(4))             
            n1 => V(1:3,IXS(Gnode(1,SECID)+1,ID))  
            n2 => V(1:3,IXS(Gnode(2,SECID)+1,ID))  
            n3 => V(1:3,IXS(Gnode(3,SECID)+1,ID))             
            n4 => V(1:3,IXS(Gnode(4,SECID)+1,ID))             
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(1),ICELL) = FOURTH * (n4+n3+n2+n1)
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(2),ICELL) = FOURTH * (n1+n2+a2+a1)
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(3),ICELL) = FOURTH * (n2+n3+a3+a2)
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(4),ICELL) = FOURTH * (n3+n4+a4+a3)
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(5),ICELL) = FOURTH * (n4+n1+a1+a4)       
            !BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Vel(1:3) = FOURTH * (a1+a2+a3+a4)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%Vel(1:3) = FOURTH * (a1+a2+a3+a4)    
            
          ELSEIF(SECTYPE(1:6)=='POLY4 ')THEN
            nFACE = F_POLY4          
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,iPOSe(4))
            a5 => EDGE_LIST(NIN,IAD(5))%CUTPOINT(1:3,iPOSe(5))
            a6 => EDGE_LIST(NIN,IAD(6))%CUTPOINT(1:3,iPOSe(6))
            n1 => X(1:3,IXS(Gnode(1,SECID)+1,ID))
            n2 => X(1:3,IXS(Gnode(2,SECID)+1,ID))
            n3 => X(1:3,IXS(Gnode(3,SECID)+1,ID))
            n4 => X(1:3,IXS(Gnode(4,SECID)+1,ID))
            Fa(1:6) = Gface(1:6,SECID)
            vF(:,1) =I22AERA(3,(/n1,a2,a1/)      ,   C(1:3,Fa(1)))
            vF(:,2) =I22AERA(5,(/n2,n4,a5,a4,n3/),   C(1:3,Fa(2)))
            vF(:,3) =I22AERA(5,(/a1,a6,n4,n2,n1/),   C(1:3,Fa(3)))
            vF(:,4) =I22AERA(5,(/n1,n2,n3,a3,a2/),   C(1:3,Fa(4)))
            vF(:,5) =I22AERA(3,(/n4,a6,a5/),         C(1:3,Fa(5)))
            vF(:,6) =I22AERA(3,(/n3,a4,a3/),         C(1:3,Fa(6)))
            vF(:,0) =I22AERA(6,(/a1,a2,a3,a4,a5,a6/),C(1:3,   0 ))
            VOL = SUM((/(vF(:,I)*C(1:3,Fa(I)),I=0,6)/))
            VOL = THIRD * ABS(VOL)
            if(icell==2)then
              vol1 = brick_list(nin,ib)%poly(1)%vnew
              vol2 = vol
              if((vol1+vol2)/uncutvol>=ONE-EM04)then
                ITAG(ICELL)=1 
                NINTPOINT = NINTPOINT - NTRUS
                !print *,"cleaning 3"
                cycle
              elseif( ABS((vol1-vol2)/uncutvol)<=EM04 )then
                scut1 = BRICK_LIST(NIN,IB)%Pcut(1)%Scut(1)
                scut2 = BRICK_LIST(NIN,IB)%Pcut(2)%Scut(1)
                tmp(1) = C(1,   0 ) - BRICK_LIST(NIN,IB)%POLY(1)%FACE0%Center(1)
                tmp(2) = C(2,   0 ) - BRICK_LIST(NIN,IB)%POLY(1)%FACE0%Center(2)
                tmp(3) = C(3,   0 ) - BRICK_LIST(NIN,IB)%POLY(1)%FACE0%Center(3)
                dist  = tmp(1)*tmp(1)+tmp(2)*tmp(2)+tmp(3)*tmp(3)
                dist = sqrt(dist)
                if (dist / EXP(0.333*LOG(UNCUTVOL)) <= EM04)THEN
                  ITAG(ICELL)=1 
                  NINTPOINT = NINTPOINT - NTRUS
                  !print *,"cleaning 4"
                  cycle                
                ENDIF
              endif
            endif
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3) + a2 + a1 !put sum intersection point in icell 9 (will be used in sinit22_fvm.F to compute faceCenter for poly9
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3) + a5 + a4
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3) + a1 + a6             
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(4))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(4))%Center(1:3) + a3 + a2              
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(5))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(5))%Center(1:3) + a6 + a5                         
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(6))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(6))%Center(1:3) + a4 + a3                        
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%Center(1:3) = C(1:3,Fa(1))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%Center(1:3) = C(1:3,Fa(2))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%Center(1:3) = C(1:3,Fa(3))                                    
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(4))%Center(1:3) = C(1:3,Fa(4))    
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(5))%Center(1:3) = C(1:3,Fa(5))             
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(6))%Center(1:3) = C(1:3,Fa(6)) 
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%Center(1:3)       = C(1:3,   0 )                         
            SCUT = SQRT(SUM(vF(:,0)*vF(:,0))) 
            pFACE(1:6) => BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)!%Surf
            pFACE0=> BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Surf
            pFACE(Fa(1))%Surf = SQRT(SUM(vF(:,1)*vF(:,1)))
            pFACE(Fa(2))%Surf = SQRT(SUM(vF(:,2)*vF(:,2)))
            pFACE(Fa(3))%Surf = SQRT(SUM(vF(:,3)*vF(:,3)))
            pFACE(Fa(4))%Surf = SQRT(SUM(vF(:,4)*vF(:,4)))
            pFACE(Fa(5))%Surf = SQRT(SUM(vF(:,5)*vF(:,5)))
            pFACE(Fa(6))%Surf = SQRT(SUM(vF(:,6)*vF(:,6)))
            pFACE0         = SCUT
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%SCUT   = SCUT                         !aire de la surface de coupure    
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%B(1:3) = C(1:3,0)                     !centroid de la surface de coupure (i.e. basis point)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%N(1:3) = vF(:,0)                      !Cut surface normal vector (not unitary : 2S factor)          
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%NP     = 6                            !polygon points (number)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,1)= a1                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,2)= a2                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,3)= a3                          !polygon points (coordinates)           
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,4)= a4                          !polygon points (coordinates)          
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,5)= a5                          !polygon points (coordinates)           
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,6)= a6                          !polygon points (coordinates)           
            pSUBVOL(ICELL)%Vnew = VOL
            pNNodCell(ICELL)%NumNOD = MNOD !main(brick nodes)  
            pNPointCell(ICELL)%NumPOINT= NNOD                
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(1)=SUM((/a1(1),a2(1),a3(1),a4(1),a5(1),a6(1),n1(1),n2(1),n3(1),n4(1)/))/TEN
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(2)=SUM((/a1(2),a2(2),a3(2),a4(2),a5(2),a6(2),n1(2),n2(2),n3(2),n4(2)/))/TEN
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(3)=SUM((/a1(3),a2(3),a3(3),a4(3),a5(3),a6(3),n1(3),n2(3),n3(3),n4(3)/))/TEN 
            BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:MNOD) = Gnode(1:MNOD,SECID) !IXS(Gnode(1,SECID)+1,ID)
            INodTAG(Gnode(1:MNOD,SECID)) = 1 !tag local brick nodes [1 to 8] if used by polyhedron   
            pWhichCellNod(Gnode(1:MNOD,SECID))%WhichCell = ICELL 
            !VELOCITY ON POLYHEDRA FACES
            a1 => EDGE_LIST(NIN,IAD(1))%CUTVEL(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTVEL(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTVEL(1:3,iPOSe(3)) 
            a4 => EDGE_LIST(NIN,IAD(4))%CUTVEL(1:3,iPOSe(4))             
            a5 => EDGE_LIST(NIN,IAD(5))%CUTVEL(1:3,iPOSe(5)) 
            a6 => EDGE_LIST(NIN,IAD(6))%CUTVEL(1:3,iPOSe(6))                         
            n1 => V(1:3,IXS(Gnode(1,SECID)+1,ID))  
            n2 => V(1:3,IXS(Gnode(2,SECID)+1,ID))  
            n3 => V(1:3,IXS(Gnode(3,SECID)+1,ID))             
            n4 => V(1:3,IXS(Gnode(4,SECID)+1,ID))             
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(1),ICELL) = THIRD  * (n1+a2+a1)      
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(2),ICELL) = ONE_FIFTH * (n2+n4+a5+a4+n3)
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(3),ICELL) = ONE_FIFTH * (a1+a6+n4+n2+n1)
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(4),ICELL) = ONE_FIFTH * (n1+n2+n3+a3+a2)
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(5),ICELL) = THIRD  * (n4+a6+a5)            
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(6),ICELL) = THIRD  * (n3+a4+a3)                        
            !BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Vel(1:3) = ONE_OVER_6  * (a1+a2+a3+a4+a5+a6 )
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%Vel(1:3) = ONE_OVER_6  * (a1+a2+a3+a4+a5+a6 )          

          ELSEIF(SECTYPE(1:6)=='POLY4A')THEN
            nFACE = F_POLY4A       
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,iPOSe(4))
            a5 => EDGE_LIST(NIN,IAD(5))%CUTPOINT(1:3,iPOSe(5))
            a6 => EDGE_LIST(NIN,IAD(6))%CUTPOINT(1:3,iPOSe(6))
            n1 => X(1:3,IXS(Gnode(1,SECID)+1,ID))
            n2 => X(1:3,IXS(Gnode(2,SECID)+1,ID))
            n3 => X(1:3,IXS(Gnode(3,SECID)+1,ID))
            n4 => X(1:3,IXS(Gnode(4,SECID)+1,ID))
            Fa(1:6) = Gface(1:6,SECID)
            vF(:,1) =I22AERA(3,(/n1,a1,a6/)      ,   C(1:3,Fa(1)))
            vF(:,2) =I22AERA(4,(/n1,n2,a2,a1/)   ,   C(1:3,Fa(2)))
            vF(:,3) =I22AERA(5,(/n2,n3,n4,a3,a2/),   C(1:3,Fa(3)))
            vF(:,4) =I22AERA(3,(/n4,a4,a3/)      ,   C(1:3,Fa(4)))
            vF(:,5) =I22AERA(4,(/n4,n3,a5,a4/)   ,   C(1:3,Fa(5)))
            vF(:,6) =I22AERA(5,(/n3,n2,n1,a6,a5/),   C(1:3,Fa(6)))
            vF(:,0) =I22AERA(6,(/a1,a2,a3,a4,a5,a6/),C(1:3,   0 ))
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3) + a1 + a6 !put sum intersection point in icell 9 (will be used in sinit22_fvm.F to compute faceCenter for poly9
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3) + a2 + a1
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3) + a3 + a2             
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(4))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(4))%Center(1:3) + a4 + a3              
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(5))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(5))%Center(1:3) + a5 + a4                         
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(6))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(6))%Center(1:3) + a6 + a5             
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%Center(1:3) = C(1:3,Fa(1))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%Center(1:3) = C(1:3,Fa(2))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%Center(1:3) = C(1:3,Fa(3))                                    
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(4))%Center(1:3) = C(1:3,Fa(4))    
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(5))%Center(1:3) = C(1:3,Fa(5))             
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(6))%Center(1:3) = C(1:3,Fa(6))  
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%Center(1:3)       = C(1:3,   0 )                                     
            SCUT = SQRT(SUM(vF(:,0)*vF(:,0))) 
            pFACE(1:6) => BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)!%Surf
            pFACE0=> BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Surf         
            pFACE(Fa(1))%Surf = SQRT(SUM(vF(:,1)*vF(:,1)))
            pFACE(Fa(2))%Surf = SQRT(SUM(vF(:,2)*vF(:,2)))
            pFACE(Fa(3))%Surf = SQRT(SUM(vF(:,3)*vF(:,3)))
            pFACE(Fa(4))%Surf = SQRT(SUM(vF(:,4)*vF(:,4)))
            pFACE(Fa(5))%Surf = SQRT(SUM(vF(:,5)*vF(:,5)))
            pFACE(Fa(6))%Surf = SQRT(SUM(vF(:,6)*vF(:,6)))
            pFACE0         = SCUT
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%SCUT    = SCUT                        !aire de la surface de coupure    
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%B(1:3)  = C(1:3,0)                    !centroid de la surface de coupure (i.e. basis point)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%N(1:3)  = vF(:,0)                     !Cut surface normal vector (not unitary : 2S factor)          
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%NP      = 6                           !polygon points (number)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,1)= a1                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,2)= a2                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,3)= a3                          !polygon points (coordinates)           
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,4)= a4                          !polygon points (coordinates)          
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,5)= a5                          !polygon points (coordinates)           
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,6)= a6                          !polygon points (coordinates)           
            VOL = SUM((/(vF(:,I)*C(1:3,Fa(I)),I=0,6)/))
            VOL = THIRD * ABS(VOL)
            pSUBVOL(ICELL)%Vnew = VOL
            pNNodCell(ICELL)%NumNOD = MNOD !main(brick nodes)  
            pNPointCell(ICELL)%NumPOINT= NNOD                
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(1)= SUM((/a1(1),a2(1),a3(1),a4(1),a5(1),a6(1),n1(1),n2(1),n3(1),n4(1)/))/TEN
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(2)= SUM((/a1(2),a2(2),a3(2),a4(2),a5(2),a6(2),n1(2),n2(2),n3(2),n4(2)/))/TEN
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(3)= SUM((/a1(3),a2(3),a3(3),a4(3),a5(3),a6(3),n1(3),n2(3),n3(3),n4(3)/))/TEN 
            BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:MNOD) = Gnode(1:MNOD,SECID) !IXS(Gnode(1,SECID)+1,ID)
            INodTAG(Gnode(1:MNOD,SECID)) = 1 !tag local brick nodes [1 to 8] if used by polyhedron   
            pWhichCellNod(Gnode(1:MNOD,SECID))%WhichCell = ICELL   
            !VELOCITY ON POLYHEDRA FACES
            a1 => EDGE_LIST(NIN,IAD(1))%CUTVEL(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTVEL(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTVEL(1:3,iPOSe(3)) 
            a4 => EDGE_LIST(NIN,IAD(4))%CUTVEL(1:3,iPOSe(4))             
            a5 => EDGE_LIST(NIN,IAD(5))%CUTVEL(1:3,iPOSe(5)) 
            a6 => EDGE_LIST(NIN,IAD(6))%CUTVEL(1:3,iPOSe(6))                         
            n1 => V(1:3,IXS(Gnode(1,SECID)+1,ID))  
            n2 => V(1:3,IXS(Gnode(2,SECID)+1,ID))  
            n3 => V(1:3,IXS(Gnode(3,SECID)+1,ID))             
            n4 => V(1:3,IXS(Gnode(4,SECID)+1,ID))             
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(1),ICELL) = THIRD  * (n1+a1+a6)       
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(2),ICELL) = FOURTH  * (n1+n2+a2+a1)    
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(3),ICELL) = ONE_FIFTH * (n2+n3+n4+a3+a2) 
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(4),ICELL) = THIRD  * (n4+a4+a3)       
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(5),ICELL) = FOURTH  * (n4+n3+a5+a4)        
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(6),ICELL) = ONE_FIFTH * (n3+n2+n1+a6+a5)                 
            !BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Vel(1:3) = ONE_OVER_6  * (a1+a2+a3+a4+a5+a6 )  
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%Vel(1:3) = ONE_OVER_6  * (a1+a2+a3+a4+a5+a6 )          

          ELSEIF(SECTYPE(1:6)=='POLY4B')THEN
            nFACE = F_POLY4B
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,iPOSe(4))
            a5 => EDGE_LIST(NIN,IAD(5))%CUTPOINT(1:3,iPOSe(5))
            a6 => EDGE_LIST(NIN,IAD(6))%CUTPOINT(1:3,iPOSe(6))
            n1 => X(1:3,IXS(Gnode(1,SECID)+1,ID))
            n2 => X(1:3,IXS(Gnode(2,SECID)+1,ID))
            n3 => X(1:3,IXS(Gnode(3,SECID)+1,ID))
            n4 => X(1:3,IXS(Gnode(4,SECID)+1,ID))
            Fa(1:6) = Gface(1:6,SECID)
            vF(:,1) = - I22AERA(3,(/n1,a1,a6/)      ,   C(1:3,Fa(1)))
            vF(:,2) = - I22AERA(4,(/n1,n2,a2,a1/)   ,   C(1:3,Fa(2)))
            vF(:,3) = - I22AERA(5,(/n2,n3,n4,a3,a2/),   C(1:3,Fa(3)))
            vF(:,4) = - I22AERA(3,(/n4,a4,a3/)      ,   C(1:3,Fa(4)))
            vF(:,5) = - I22AERA(4,(/n4,n3,a5,a4/)   ,   C(1:3,Fa(5)))
            vF(:,6) = - I22AERA(5,(/n3,n2,n1,a6,a5/),   C(1:3,Fa(6)))
            vF(:,0) = - I22AERA(6,(/a1,a2,a3,a4,a5,a6/),C(1:3,   0 ))
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3) + a1 + a6 !put sum intersection point in icell 9 (will be used in sinit22_fvm.F to compute faceCenter for poly9
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3) + a2 + a1
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3) + a3 + a2             
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(4))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(4))%Center(1:3) + a4 + a3              
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(5))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(5))%Center(1:3) + a5 + a4                         
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(6))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(6))%Center(1:3) + a6 + a5             
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%Center(1:3) = C(1:3,Fa(1))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%Center(1:3) = C(1:3,Fa(2))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%Center(1:3) = C(1:3,Fa(3))                                    
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(4))%Center(1:3) = C(1:3,Fa(4))    
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(5))%Center(1:3) = C(1:3,Fa(5))             
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(6))%Center(1:3) = C(1:3,Fa(6))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%Center(1:3)       = C(1:3,   0 )              
            SCUT = SQRT(SUM(vF(:,0)*vF(:,0)))  
            pFACE(1:6) => BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)!%Surf
            pFACE0=> BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Surf        
            pFACE(Fa(1))%Surf = SQRT(SUM(vF(:,1)*vF(:,1)))
            pFACE(Fa(2))%Surf = SQRT(SUM(vF(:,2)*vF(:,2)))
            pFACE(Fa(3))%Surf = SQRT(SUM(vF(:,3)*vF(:,3)))
            pFACE(Fa(4))%Surf = SQRT(SUM(vF(:,4)*vF(:,4)))
            pFACE(Fa(5))%Surf = SQRT(SUM(vF(:,5)*vF(:,5)))
            pFACE(Fa(6))%Surf = SQRT(SUM(vF(:,6)*vF(:,6)))
            pFACE0         = SCUT
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%SCUT    = SCUT                        !aire de la surface de coupure    
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%B(1:3)  = C(1:3,0)                    !centroid de la surface de coupure (i.e. basis point)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%N(1:3)  = vF(:,0)                     !Cut surface normal vector (not unitary : 2S factor)          
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%NP      = 6                           !polygon points (number)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,1)= a1                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,2)= a2                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,3)= a3                          !polygon points (coordinates)           
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,4)= a4                          !polygon points (coordinates)          
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,5)= a5                          !polygon points (coordinates)           
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,6)= a6                          !polygon points (coordinates)           
            VOL = SUM((/(vF(:,I)*C(1:3,Fa(I)),I=0,6)/))
            VOL = THIRD * ABS(VOL)
            pSUBVOL(ICELL)%Vnew = VOL
            pNNodCell(ICELL)%NumNOD = MNOD !main(brick nodes)  
            pNPointCell(ICELL)%NumPOINT= NNOD                
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(1)=SUM((/a1(1),a2(1),a3(1),a4(1),a5(1),a6(1),n1(1),n2(1),n3(1),n4(1)/))/TEN
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(2)=SUM((/a1(2),a2(2),a3(2),a4(2),a5(2),a6(2),n1(2),n2(2),n3(2),n4(2)/))/TEN
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(3)=SUM((/a1(3),a2(3),a3(3),a4(3),a5(3),a6(3),n1(3),n2(3),n3(3),n4(3)/))/TEN 
            BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:MNOD) = Gnode(1:MNOD,SECID) !IXS(Gnode(1,SECID)+1,ID)
            INodTAG(Gnode(1:MNOD,SECID)) = 1 !tag local brick nodes [1 to 8] if used by polyhedron   
            pWhichCellNod(Gnode(1:MNOD,SECID))%WhichCell = ICELL 
            !VELOCITY ON POLYHEDRA FACES
            a1 => EDGE_LIST(NIN,IAD(1))%CUTVEL(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTVEL(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTVEL(1:3,iPOSe(3)) 
            a4 => EDGE_LIST(NIN,IAD(4))%CUTVEL(1:3,iPOSe(4))             
            a5 => EDGE_LIST(NIN,IAD(5))%CUTVEL(1:3,iPOSe(5)) 
            a6 => EDGE_LIST(NIN,IAD(6))%CUTVEL(1:3,iPOSe(6))                         
            n1 => V(1:3,IXS(Gnode(1,SECID)+1,ID))  
            n2 => V(1:3,IXS(Gnode(2,SECID)+1,ID))  
            n3 => V(1:3,IXS(Gnode(3,SECID)+1,ID))             
            n4 => V(1:3,IXS(Gnode(4,SECID)+1,ID))             
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(1),ICELL) = THIRD  * (n1+a1+a6)      
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(2),ICELL) = FOURTH  * (n1+n2+a2+a1)   
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(3),ICELL) = ONE_FIFTH * (n2+n3+n4+a3+a2)
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(4),ICELL) = THIRD  * (n4+a4+a3)      
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(5),ICELL) = FOURTH  * (n4+n3+a5+a4)      
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(6),ICELL) = ONE_FIFTH * (n3+n2+n1+a6+a5)               
            !BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Vel(1:3) = ONE_OVER_6  * (a1+a2+a3+a4+a5+a6 ) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%Vel(1:3) = ONE_OVER_6  * (a1+a2+a3+a4+a5+a6 )             
                        
          ELSEIF(SECTYPE(1:5)=='POLYC')THEN
            nFACE = F_POLYC     
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,iPOSe(4))
            a5 => EDGE_LIST(NIN,IAD(5))%CUTPOINT(1:3,iPOSe(5))  !points 5,6 on same edge : IAD(5)=IAD(6) BUT ICUT=1
            a6 => EDGE_LIST(NIN,IAD(6))%CUTPOINT(1:3,iPOSe(6))  !points 5,6 on same edge : IAD(5)=IAD(6) BUT ICUT=2
            n1 => X(1:3,IXS(Gnode(1,SECID)+1,ID))
            n2 => X(1:3,IXS(Gnode(2,SECID)+1,ID))
            n3 => X(1:3,IXS(Gnode(3,SECID)+1,ID))
            n4 => X(1:3,IXS(Gnode(4,SECID)+1,ID))
            Fa(1:6) = Gface(1:6,SECID)
            vF(:,1) =I22AERA(4,(/n1,n2,n3,n4/)          ,   C(1:3,Fa(1)))
            vF(:,2) =I22AERA(4,(/n1,a1,a2,n2/)          ,   C(1:3,Fa(2)))
            vF(:,3) =I22AERA(4,(/n2,a2,a3,n3/)          ,   C(1:3,Fa(3)))
            vF(:,4) =I22AERA(4,(/n1,n4,a4,a1/)          ,   C(1:3,Fa(4)))
            vF(:,5) =I22AERA(7,(/n4,n3,a3,a6,a5,a4,n4/) ,   C(1:3,Fa(5)))
            vF(:,0) =I22AERA(6,(/a1,a4,a5,a6,a3,a2/)    ,   C(1:3,   0 ))  !TO BE UPDATED LATER IF NEEED : this is an approximation
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(1))%Center(1:3) + ZERO    !put sum intersection point in icell 9 (will be used in sinit22_fvm.F to compute faceCenter for poly9
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(2))%Center(1:3) +a1+a2
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(3))%Center(1:3) +a2+a3             
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(4))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(4))%Center(1:3) +a4+a1              
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(5))%Center(1:3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(Fa(5))%Center(1:3) +a3+a6+a5+a4                                     
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%Center(1:3) = C(1:3,Fa(1))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%Center(1:3) = C(1:3,Fa(2))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%Center(1:3) = C(1:3,Fa(3))                                    
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(4))%Center(1:3) = C(1:3,Fa(4))    
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(5))%Center(1:3) = C(1:3,Fa(5))
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%Center(1:3)       = C(1:3,   0 )              
            SCUT = SQRT(SUM(vF(:,0)*vF(:,0)))  
            pFACE(1:6) => BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)!%Surf
            pFACE0=> BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Surf        
            pFACE(Fa(1))%Surf = SQRT(SUM(vF(:,1)*vF(:,1)))
            pFACE(Fa(2))%Surf = SQRT(SUM(vF(:,2)*vF(:,2)))
            pFACE(Fa(3))%Surf = SQRT(SUM(vF(:,3)*vF(:,3)))
            pFACE(Fa(4))%Surf = SQRT(SUM(vF(:,4)*vF(:,4)))
            pFACE(Fa(5))%Surf = SQRT(SUM(vF(:,5)*vF(:,5)))
            pFACE0         = SCUT
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%SCUT   = SCUT                         !aire de la surface de coupure    
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%B(1:3) = C(1:3,0)                     !centroid de la surface de coupure (i.e. basis point)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%N(1:3) = vF(:,0)                      !Cut surface normal vector (not unitary : 2S factor)          
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%NP     = 6                            !polygon points (number)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,1)= a1                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,2)= a2                          !polygon points (coordinates) 
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,3)= a3                          !polygon points (coordinates)           
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,4)= a4                          !polygon points (coordinates)          
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,5)= a5                          !polygon points (coordinates)           
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,6)= a6                          !polygon points (coordinates)           
            VOL = SUM((/(vF(:,I)*C(1:3,Fa(I)),I=0,5)/))
            VOL = THIRD * ABS(VOL)
            pSUBVOL(ICELL)%Vnew =  VOL
            pNNodCell(ICELL)%NumNOD = MNOD !main(brick nodes)  
            pNPointCell(ICELL)%NumPOINT= NNOD                
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(1)=SUM((/a1(1),a2(1),a3(1),a4(1),a5(1),a6(1),n1(1),n2(1),n3(1),n4(1)/))/TEN
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(2)=SUM((/a1(2),a2(2),a3(2),a4(2),a5(2),a6(2),n1(2),n2(2),n3(2),n4(2)/))/TEN
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(3)=SUM((/a1(3),a2(3),a3(3),a4(3),a5(3),a6(3),n1(3),n2(3),n3(3),n4(3)/))/TEN   
            BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:MNOD) = Gnode(1:MNOD,SECID) !IXS(Gnode(1,SECID)+1,ID)
            INodTAG(Gnode(1:MNOD,SECID)) = 1 !tag local brick nodes [1 to 8] if used by polyhedron   
            pWhichCellNod(Gnode(1:MNOD,SECID))%WhichCell = ICELL  
            !VELOCITY ON POLYHEDRA FACES
            a1 => EDGE_LIST(NIN,IAD(1))%CUTVEL(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTVEL(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTVEL(1:3,iPOSe(3)) 
            a4 => EDGE_LIST(NIN,IAD(4))%CUTVEL(1:3,iPOSe(4))             
            a5 => EDGE_LIST(NIN,IAD(5))%CUTVEL(1:3,iPOSe(5)) 
            a6 => EDGE_LIST(NIN,IAD(6))%CUTVEL(1:3,iPOSe(6))                         
            n1 => V(1:3,IXS(Gnode(1,SECID)+1,ID))  
            n2 => V(1:3,IXS(Gnode(2,SECID)+1,ID))  
            n3 => V(1:3,IXS(Gnode(3,SECID)+1,ID))             
            n4 => V(1:3,IXS(Gnode(4,SECID)+1,ID))             
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(1),ICELL) = FOURTH * (n1+n2+n3+n4)       
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(2),ICELL) = FOURTH * (n1+a1+a2+n2)       
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(3),ICELL) = FOURTH * (n2+a2+a3+n3)       
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(4),ICELL) = FOURTH * (n1+n4+a4+a1)       
            !!BRICK_LIST(NIN,IB)%Vel_Face(1:3,Fa(5),ICELL) =         (n4+n3+a3+a6+a5+a4+n4) / SEVEN          
            !BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Vel(1:3) = ONE_OVER_6 * (a1+a4+a5+a6+a3+a2)   
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%Vel(1:3) = ONE_OVER_6 * (a1+a4+a5+a6+a3+a2)                                                                       
          END IF
          
          !Treating complementary polyhedron (exemple : icode=2842;idble=2312)
          IF(bCOMPL)THEN
            pFACE(1:6) => BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)!%Surf
            pFACE0=> BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Surf
            DO J=1,nFACE
              pFACE(Fa(J))%Surf =- pFACE(Fa(J))%Surf
            ENDDO
            pSUBVOL(ICELL)%Vnew       =- pSUBVOL(ICELL)%Vnew   
            pNNodCell(ICELL)%NumNOD     =  8-MNOD                   
          ENDIF

          !------------------------------------------------------------!
          !           GRID VELOCITIES ON POLYEDRA FACES                !
          !------------------------------------------------------------!
          BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%W(1:3)       = ZERO  
          BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%W(1:3) = ZERO
          IF(I22_ALEUL == 1)THEN
          IF(SECTYPE(1:5)=='TETRA')THEN
            nFACE   = F_TETRA
            Fa(1:3) = Gface(1:3,SECID)
            !intersection points
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            !interpolate w_a1
            beta    = EDGE_LIST(NIN,IAD(1))%CUTCOOR(iPOSe(1));
            J       = IAD(1)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa1(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa1(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa1(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a2
            beta    = EDGE_LIST(NIN,IAD(2))%CUTCOOR(iPOSe(2));
            J       = IAD(2)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa2(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa2(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa2(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a3
            beta    = EDGE_LIST(NIN,IAD(3))%CUTCOOR(iPOSe(3));
            J       = IAD(3)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa3(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa3(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa3(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !brick nodes                       
            n1    => X(1:3,IXS(Gnode(1,SECID)+1,ID))
            wn1(1)= W(1,IXS(Gnode(1,SECID)+1,ID))
            wn1(2)= W(2,IXS(Gnode(1,SECID)+1,ID))
            wn1(3)= W(3,IXS(Gnode(1,SECID)+1,ID))
            !Grid velocity on face1 :
            wf1(1) = THIRD*(wn1(1)+wa2(1)+wa1(1))
            wf1(2) = THIRD*(wn1(2)+wa2(2)+wa1(2))
            wf1(3) = THIRD*(wn1(3)+wa2(3)+wa1(3))
            !Grid velocity on face2 :
            wf2(1) = THIRD*(wn1(1)+wa3(1)+wa2(1))
            wf2(2) = THIRD*(wn1(2)+wa3(2)+wa2(2))
            wf2(3) = THIRD*(wn1(3)+wa3(3)+wa2(3))            
            !Grid velocity on face3 :
            wf3(1) = THIRD*(wn1(1)+wa1(1)+wa3(1))
            wf3(2) = THIRD*(wn1(2)+wa1(2)+wa3(2))
            wf3(3) = THIRD*(wn1(3)+wa1(3)+wa3(3)) 
            !Grid velocity on face0 :
            wf0(1) = THIRD*(wa1(1)+wa2(1)+wa3(1))
            wf0(2) = THIRD*(wa1(2)+wa2(2)+wa3(2))
            wf0(3) = THIRD*(wa1(3)+wa2(3)+wa3(3)) 
            !storage
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%W(1:3)       = wf0(1:3)  
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%W(1:3) = wf1(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%W(1:3) = wf2(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%W(1:3) = wf3(1:3)              


          ELSEIF(SECTYPE(1:5)=='PENTA')THEN
            nFACE   = F_PENTA 
            Fa(1:4) = Gface(1:4,SECID)    
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,iPOSe(4))
            n1 => X(1:3,IXS(Gnode(1,SECID)+1,ID))
            n2 => X(1:3,IXS(Gnode(2,SECID)+1,ID))
            !interpolate w_a1
            beta    = EDGE_LIST(NIN,IAD(1))%CUTCOOR(iPOSe(1));
            J       = IAD(1)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa1(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa1(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa1(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a2
            beta    = EDGE_LIST(NIN,IAD(2))%CUTCOOR(iPOSe(2));
            J       = IAD(2)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa2(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa2(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa2(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a3
            beta    = EDGE_LIST(NIN,IAD(3))%CUTCOOR(iPOSe(3));
            J       = IAD(3)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa3(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa3(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa3(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a4
            beta    = EDGE_LIST(NIN,IAD(4))%CUTCOOR(iPOSe(4));
            J       = IAD(4)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa4(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa4(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa4(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !brick nodes                       
            wn1(1)= W(1,IXS(Gnode(1,SECID)+1,ID))
            wn1(2)= W(2,IXS(Gnode(1,SECID)+1,ID))
            wn1(3)= W(3,IXS(Gnode(1,SECID)+1,ID))
            !brick nodes                       
            wn2(1)= W(1,IXS(Gnode(2,SECID)+1,ID))
            wn2(2)= W(2,IXS(Gnode(2,SECID)+1,ID))
            wn2(3)= W(3,IXS(Gnode(2,SECID)+1,ID))            
            !Grid velocity on face1 :
            wf1(1) = THIRD*(wn1(1)+wa2(1)+wa1(1))
            wf1(2) = THIRD*(wn1(2)+wa2(2)+wa1(2))
            wf1(3) = THIRD*(wn1(3)+wa2(3)+wa1(3))            
            !Grid velocity on face2 :
            wf2(1) = THIRD*(wn2(1)+wa4(1)+wa3(1))
            wf2(2) = THIRD*(wn2(2)+wa4(2)+wa3(2))
            wf2(3) = THIRD*(wn2(3)+wa4(3)+wa3(3))                     
            !Grid velocity on face3 :
            wf3(1) = FOURTH*(wn1(1)+wn2(1)+wa3(1)+wa2(1))
            wf3(2) = FOURTH*(wn1(2)+wn2(2)+wa3(2)+wa2(2))
            wf3(3) = FOURTH*(wn1(3)+wn2(3)+wa3(3)+wa2(3)) 
            !Grid velocity on face4 :
            wf4(1) = FOURTH*(wn2(1)+wn1(1)+wa1(1)+wa4(1))
            wf4(2) = FOURTH*(wn2(2)+wn1(2)+wa1(2)+wa4(2))
            wf4(3) = FOURTH*(wn2(3)+wn1(3)+wa1(3)+wa4(3))             
            !Grid velocity on face0 :
            wf0(1) = FOURTH*(wa1(1)+wa2(1)+wa3(1)+wa4(1))
            wf0(2) = FOURTH*(wa1(2)+wa2(2)+wa3(2)+wa4(2))
            wf0(3) = FOURTH*(wa1(3)+wa2(3)+wa3(3)+wa4(3)) 
            !storage
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%W(1:3)       = wf0(1:3)  
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%W(1:3) = wf1(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%W(1:3) = wf2(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%W(1:3) = wf3(1:3)                      
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(4))%W(1:3) = wf4(1:3)  

          ELSEIF(SECTYPE(1:5)=='POLY3')THEN
            nFACE   = F_POLY3  
            Fa(1:5) = Gface(1:5,SECID)
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,iPOSe(4))
            a5 => EDGE_LIST(NIN,IAD(5))%CUTPOINT(1:3,iPOSe(5))
            n1 => X(1:3,IXS(Gnode(1,SECID)+1,ID))
            n2 => X(1:3,IXS(Gnode(2,SECID)+1,ID))
            n3 => X(1:3,IXS(Gnode(3,SECID)+1,ID))
            !interpolate w_a1
            beta    = EDGE_LIST(NIN,IAD(1))%CUTCOOR(iPOSe(1));
            J       = IAD(1)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa1(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa1(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa1(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a2
            beta    = EDGE_LIST(NIN,IAD(2))%CUTCOOR(iPOSe(2));
            J       = IAD(2)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa2(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa2(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa2(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a3
            beta    = EDGE_LIST(NIN,IAD(3))%CUTCOOR(iPOSe(3));
            J       = IAD(3)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa3(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa3(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa3(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a4
            beta    = EDGE_LIST(NIN,IAD(4))%CUTCOOR(iPOSe(4));
            J       = IAD(4)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa4(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa4(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa4(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a5
            beta    = EDGE_LIST(NIN,IAD(4))%CUTCOOR(iPOSe(5));
            J       = IAD(5)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa5(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa5(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa5(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !brick nodes                       
            wn1(1)= W(1,IXS(Gnode(1,SECID)+1,ID))
            wn1(2)= W(2,IXS(Gnode(1,SECID)+1,ID))
            wn1(3)= W(3,IXS(Gnode(1,SECID)+1,ID))
            !brick nodes                       
            wn2(1)= W(1,IXS(Gnode(2,SECID)+1,ID))
            wn2(2)= W(2,IXS(Gnode(2,SECID)+1,ID))
            wn2(3)= W(3,IXS(Gnode(2,SECID)+1,ID))   
            !brick nodes                       
            wn3(1)= W(1,IXS(Gnode(3,SECID)+1,ID))
            wn3(2)= W(2,IXS(Gnode(3,SECID)+1,ID))
            wn3(3)= W(3,IXS(Gnode(3,SECID)+1,ID))                      
            !Grid velocity on face1 :
            wf1(1) = ONE_FIFTH*(wn1(1)+wn2(1)+wn3(1)+wa3(1)+wa2(1))
            wf1(2) = ONE_FIFTH*(wn1(2)+wn2(2)+wn3(2)+wa3(2)+wa2(2))
            wf1(3) = ONE_FIFTH*(wn1(3)+wn2(3)+wn3(3)+wa3(3)+wa2(3))                       
            !Grid velocity on face2 :
            wf2(1) = THIRD*(wn3(1)+wa4(1)+wa3(1))
            wf2(2) = THIRD*(wn3(2)+wa4(2)+wa3(2))
            wf2(3) = THIRD*(wn3(3)+wa4(3)+wa3(3))                                 
            !Grid velocity on face3 :
            wf3(1) = FOURTH*(wn3(1)+wn2(1)+wa5(1)+wa4(1))
            wf3(2) = FOURTH*(wn3(2)+wn2(2)+wa5(2)+wa4(2))
            wf3(3) = FOURTH*(wn3(3)+wn2(3)+wa5(3)+wa4(3))             
            !Grid velocity on face4 :
            wf4(1) = FOURTH*(wn2(1)+wn1(1)+wa1(1)+wa5(1))
            wf4(2) = FOURTH*(wn2(2)+wn1(2)+wa1(2)+wa5(2))
            wf4(3) = FOURTH*(wn2(3)+wn1(3)+wa1(3)+wa5(3)) 
            !Grid velocity on face5 :
            wf5(1) = THIRD*(wn1(1)+wa2(1)+wa1(1))
            wf5(2) = THIRD*(wn1(2)+wa2(2)+wa1(2))
            wf5(3) = THIRD*(wn1(3)+wa2(3)+wa1(3))                                      
            !Grid velocity on face0 :
            wf0(1) = ONE_FIFTH*(wa1(1)+wa2(1)+wa3(1)+wa4(1)+wa5(1))
            wf0(2) = ONE_FIFTH*(wa1(2)+wa2(2)+wa3(2)+wa4(2)+wa5(2))
            wf0(3) = ONE_FIFTH*(wa1(3)+wa2(3)+wa3(3)+wa4(3)+wa5(3)) 
            !storage
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%W(1:3)       = wf0(1:3)  
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%W(1:3) = wf1(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%W(1:3) = wf2(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%W(1:3) = wf3(1:3)                      
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(4))%W(1:3) = wf4(1:3)  
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(5))%W(1:3) = wf5(1:3)
            
          ELSEIF(SECTYPE(1:5)=='HEXAE')THEN
            nFACE   = F_HEXAE 
            Fa(1:5) = Gface(1:5,SECID)   
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,iPOSe(4))
            n1 => X(1:3,IXS(Gnode(1,SECID)+1,ID))
            n2 => X(1:3,IXS(Gnode(2,SECID)+1,ID))
            n3 => X(1:3,IXS(Gnode(3,SECID)+1,ID))
            n4 => X(1:3,IXS(Gnode(4,SECID)+1,ID))
            !interpolate w_a1
            beta    = EDGE_LIST(NIN,IAD(1))%CUTCOOR(iPOSe(1));
            J       = IAD(1)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa1(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa1(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa1(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a2
            beta    = EDGE_LIST(NIN,IAD(2))%CUTCOOR(iPOSe(2));
            J       = IAD(2)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa2(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa2(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa2(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a3
            beta    = EDGE_LIST(NIN,IAD(3))%CUTCOOR(iPOSe(3));
            J       = IAD(3)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa3(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa3(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa3(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a4
            beta    = EDGE_LIST(NIN,IAD(4))%CUTCOOR(iPOSe(4));
            J       = IAD(4)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa4(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa4(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa4(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !brick nodes                       
            wn1(1)= W(1,IXS(Gnode(1,SECID)+1,ID))
            wn1(2)= W(2,IXS(Gnode(1,SECID)+1,ID))
            wn1(3)= W(3,IXS(Gnode(1,SECID)+1,ID))
            !brick nodes                       
            wn2(1)= W(1,IXS(Gnode(2,SECID)+1,ID))
            wn2(2)= W(2,IXS(Gnode(2,SECID)+1,ID))
            wn2(3)= W(3,IXS(Gnode(2,SECID)+1,ID))   
            !brick nodes                       
            wn3(1)= W(1,IXS(Gnode(3,SECID)+1,ID))
            wn3(2)= W(2,IXS(Gnode(3,SECID)+1,ID))
            wn3(3)= W(3,IXS(Gnode(3,SECID)+1,ID))  
            !brick nodes                       
            wn4(1)= W(1,IXS(Gnode(4,SECID)+1,ID))
            wn4(2)= W(2,IXS(Gnode(4,SECID)+1,ID))
            wn4(3)= W(3,IXS(Gnode(4,SECID)+1,ID))                                    
            !Grid velocity on face1 :
            wf1(1) = FOURTH*(wn4(1)+wn3(1)+wn2(1)+wn1(1))
            wf1(2) = FOURTH*(wn4(2)+wn3(2)+wn2(2)+wn1(2))
            wf1(3) = FOURTH*(wn4(3)+wn3(3)+wn2(3)+wn1(3))                       
            !Grid velocity on face2 :
            wf2(1) = FOURTH*(wn1(1)+wn2(1)+wa2(1)+wa1(1))
            wf2(2) = FOURTH*(wn1(2)+wn2(2)+wa2(2)+wa1(2))
            wf2(3) = FOURTH*(wn1(3)+wn2(3)+wa2(3)+wa1(3))                                             
            !Grid velocity on face3 :
            wf3(1) = FOURTH*(wn2(1)+wn3(1)+wa3(1)+wa2(1))
            wf3(2) = FOURTH*(wn2(2)+wn3(2)+wa3(2)+wa2(2))
            wf3(3) = FOURTH*(wn2(3)+wn3(3)+wa3(3)+wa2(3))                         
            !Grid velocity on face4 :
            wf4(1) = FOURTH*(wn3(1)+wn4(1)+wa4(1)+wa3(1))
            wf4(2) = FOURTH*(wn3(2)+wn4(2)+wa4(2)+wa3(2))
            wf4(3) = FOURTH*(wn3(3)+wn4(3)+wa4(3)+wa3(3))             
            !Grid velocity on face5 :
            wf5(1) = FOURTH*(wn4(1)+wn1(1)+wa1(1)+wa4(1))
            wf5(2) = FOURTH*(wn4(2)+wn1(2)+wa1(2)+wa4(2))
            wf5(3) = FOURTH*(wn4(3)+wn1(3)+wa1(3)+wa4(3))                                               
            !Grid velocity on face0 :
            wf0(1) = ONE_FIFTH*(wa1(1)+wa2(1)+wa3(1)+wa4(1))
            wf0(2) = ONE_FIFTH*(wa1(2)+wa2(2)+wa3(2)+wa4(2))
            wf0(3) = ONE_FIFTH*(wa1(3)+wa2(3)+wa3(3)+wa4(3)) 
            !storage
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%W(1:3)       = wf0(1:3)  
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%W(1:3) = wf1(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%W(1:3) = wf2(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%W(1:3) = wf3(1:3)                      
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(4))%W(1:3) = wf4(1:3)  
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(5))%W(1:3) = wf5(1:3)            

            
          ELSEIF(SECTYPE(1:6)=='POLY4 ')THEN
            nFACE = F_POLY4  
            Fa(1:6) = Gface(1:6,SECID)        
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,iPOSe(4))
            a5 => EDGE_LIST(NIN,IAD(5))%CUTPOINT(1:3,iPOSe(5))
            a6 => EDGE_LIST(NIN,IAD(6))%CUTPOINT(1:3,iPOSe(6))
            n1 => X(1:3,IXS(Gnode(1,SECID)+1,ID))
            n2 => X(1:3,IXS(Gnode(2,SECID)+1,ID))
            n3 => X(1:3,IXS(Gnode(3,SECID)+1,ID))
            n4 => X(1:3,IXS(Gnode(4,SECID)+1,ID))
            !interpolate w_a1
            beta    = EDGE_LIST(NIN,IAD(1))%CUTCOOR(iPOSe(1));
            J       = IAD(1)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa1(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa1(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa1(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a2
            beta    = EDGE_LIST(NIN,IAD(2))%CUTCOOR(iPOSe(2));
            J       = IAD(2)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa2(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa2(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa2(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a3
            beta    = EDGE_LIST(NIN,IAD(3))%CUTCOOR(iPOSe(3));
            J       = IAD(3)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa3(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa3(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa3(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a4
            beta    = EDGE_LIST(NIN,IAD(4))%CUTCOOR(iPOSe(4));
            J       = IAD(4)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa4(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa4(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa4(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a5
            beta    = EDGE_LIST(NIN,IAD(5))%CUTCOOR(iPOSe(5));
            J       = IAD(5)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa5(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa5(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa5(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a6
            beta    = EDGE_LIST(NIN,IAD(6))%CUTCOOR(iPOSe(6));
            J       = IAD(6)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa6(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa6(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa6(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !brick nodes                       
            wn1(1)= W(1,IXS(Gnode(1,SECID)+1,ID))
            wn1(2)= W(2,IXS(Gnode(1,SECID)+1,ID))
            wn1(3)= W(3,IXS(Gnode(1,SECID)+1,ID))
            !brick nodes                       
            wn2(1)= W(1,IXS(Gnode(2,SECID)+1,ID))
            wn2(2)= W(2,IXS(Gnode(2,SECID)+1,ID))
            wn2(3)= W(3,IXS(Gnode(2,SECID)+1,ID))   
            !brick nodes                       
            wn3(1)= W(1,IXS(Gnode(3,SECID)+1,ID))
            wn3(2)= W(2,IXS(Gnode(3,SECID)+1,ID))
            wn3(3)= W(3,IXS(Gnode(3,SECID)+1,ID))  
            !brick nodes                       
            wn4(1)= W(1,IXS(Gnode(4,SECID)+1,ID))
            wn4(2)= W(2,IXS(Gnode(4,SECID)+1,ID))
            wn4(3)= W(3,IXS(Gnode(4,SECID)+1,ID))                                                
            !Grid velocity on face1 :
            wf1(1) = THIRD*(wn1(1)+wa1(1)+wa6(1))
            wf1(2) = THIRD*(wn1(2)+wa1(2)+wa6(2))
            wf1(3) = THIRD*(wn1(3)+wa1(3)+wa6(3))                                  
            !Grid velocity on face2 :
            wf2(1) = FOURTH*(wn1(1)+wn2(1)+wa2(1)+wa1(1))
            wf2(2) = FOURTH*(wn1(2)+wn2(2)+wa2(2)+wa1(2))
            wf2(3) = FOURTH*(wn1(3)+wn2(3)+wa2(3)+wa1(3))                                                        
            !Grid velocity on face3 :
            wf3(1) = ONE_FIFTH*(wn2(1)+wn3(1)+wn4(1)+wa3(1)+wa2(1))
            wf3(2) = ONE_FIFTH*(wn2(2)+wn3(2)+wn4(2)+wa3(2)+wa2(2))
            wf3(3) = ONE_FIFTH*(wn2(3)+wn3(3)+wn4(3)+wa3(3)+wa2(3))                                   
            !Grid velocity on face4 :
            wf4(1) = THIRD*(wn4(1)+wa4(1)+wa3(1))
            wf4(2) = THIRD*(wn4(2)+wa4(2)+wa3(2))
            wf4(3) = THIRD*(wn4(3)+wa4(3)+wa3(3))                        
            !Grid velocity on face5 :
            wf5(1) = FOURTH*(wn4(1)+wn3(1)+wa5(1)+wa4(1))
            wf5(2) = FOURTH*(wn4(2)+wn3(2)+wa5(2)+wa4(2))
            wf5(3) = FOURTH*(wn4(3)+wn3(3)+wa5(3)+wa4(3)) 
            !Grid velocity on face6 :
            wf6(1) = FOURTH*(wn3(1)+wn2(1)+wn1(1)+wa6(1)+wa5(1))
            wf6(2) = FOURTH*(wn3(2)+wn2(2)+wn1(2)+wa6(2)+wa5(2))
            wf6(3) = FOURTH*(wn3(3)+wn2(3)+wn1(3)+wa6(3)+wa5(3))                                                           
            !Grid velocity on face0 :
            wf0(1) = ONE_OVER_6*(wa1(1)+wa2(1)+wa3(1)+wa4(1)+wa5(1)+wa6(1))
            wf0(2) = ONE_OVER_6*(wa1(2)+wa2(2)+wa3(2)+wa4(2)+wa5(2)+wa6(2))
            wf0(3) = ONE_OVER_6*(wa1(3)+wa2(3)+wa3(3)+wa4(3)+wa5(3)+wa6(3)) 
            !storage
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%W(1:3)       = wf0(1:3)  
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%W(1:3) = wf1(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%W(1:3) = wf2(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%W(1:3) = wf3(1:3)                      
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(4))%W(1:3) = wf4(1:3)  
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(5))%W(1:3) = wf5(1:3)                 
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(6))%W(1:3) = wf6(1:3) 
                 

          ELSEIF(SECTYPE(1:6)=='POLY4A')THEN
            nFACE   = F_POLY4A  
            Fa(1:6) = Gface(1:6,SECID)     
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,iPOSe(4))
            a5 => EDGE_LIST(NIN,IAD(5))%CUTPOINT(1:3,iPOSe(5))
            a6 => EDGE_LIST(NIN,IAD(6))%CUTPOINT(1:3,iPOSe(6))
            n1 => X(1:3,IXS(Gnode(1,SECID)+1,ID))
            n2 => X(1:3,IXS(Gnode(2,SECID)+1,ID))
            n3 => X(1:3,IXS(Gnode(3,SECID)+1,ID))
            n4 => X(1:3,IXS(Gnode(4,SECID)+1,ID))
            !interpolate w_a1
            beta    = EDGE_LIST(NIN,IAD(1))%CUTCOOR(iPOSe(1));
            J       = IAD(1)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa1(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa1(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa1(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a2
            beta    = EDGE_LIST(NIN,IAD(2))%CUTCOOR(iPOSe(2));
            J       = IAD(2)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa2(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa2(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa2(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a3
            beta    = EDGE_LIST(NIN,IAD(3))%CUTCOOR(iPOSe(3));
            J       = IAD(3)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa3(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa3(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa3(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a4
            beta    = EDGE_LIST(NIN,IAD(4))%CUTCOOR(iPOSe(4));
            J       = IAD(4)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa4(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa4(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa4(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a5
            beta    = EDGE_LIST(NIN,IAD(5))%CUTCOOR(iPOSe(5));
            J       = IAD(5)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa5(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa5(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa5(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a6
            beta    = EDGE_LIST(NIN,IAD(6))%CUTCOOR(iPOSe(6));
            J       = IAD(6)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa6(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa6(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa6(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !brick nodes                       
            wn1(1)= W(1,IXS(Gnode(1,SECID)+1,ID))
            wn1(2)= W(2,IXS(Gnode(1,SECID)+1,ID))
            wn1(3)= W(3,IXS(Gnode(1,SECID)+1,ID))
            !brick nodes                       
            wn2(1)= W(1,IXS(Gnode(2,SECID)+1,ID))
            wn2(2)= W(2,IXS(Gnode(2,SECID)+1,ID))
            wn2(3)= W(3,IXS(Gnode(2,SECID)+1,ID))   
            !brick nodes                       
            wn3(1)= W(1,IXS(Gnode(3,SECID)+1,ID))
            wn3(2)= W(2,IXS(Gnode(3,SECID)+1,ID))
            wn3(3)= W(3,IXS(Gnode(3,SECID)+1,ID))  
            !brick nodes                       
            wn4(1)= W(1,IXS(Gnode(4,SECID)+1,ID))
            wn4(2)= W(2,IXS(Gnode(4,SECID)+1,ID))
            wn4(3)= W(3,IXS(Gnode(4,SECID)+1,ID))                                                            
            !Grid velocity on face1 :
            wf1(1) = THIRD*(wn1(1)+wa1(1)+wa6(1))
            wf1(2) = THIRD*(wn1(2)+wa1(2)+wa6(2))
            wf1(3) = THIRD*(wn1(3)+wa1(3)+wa6(3))                                              
            !Grid velocity on face2 :
            wf2(1) = FOURTH*(wn1(1)+wn2(1)+wa2(1)+wa1(1))
            wf2(2) = FOURTH*(wn1(2)+wn2(2)+wa2(2)+wa1(2))
            wf2(3) = FOURTH*(wn1(3)+wn2(3)+wa2(3)+wa1(3))                                                                    
            !Grid velocity on face3 :
            wf3(1) = ONE_FIFTH*(wn2(1)+wn3(1)+wn4(1)+wa3(1)+wa2(1))
            wf3(2) = ONE_FIFTH*(wn2(2)+wn3(2)+wn4(2)+wa3(2)+wa2(2))
            wf3(3) = ONE_FIFTH*(wn2(3)+wn3(3)+wn4(3)+wa3(3)+wa2(3))                                             
            !Grid velocity on face4 :
            wf4(1) = THIRD*(wn4(1)+wa4(1)+wa3(1))
            wf4(2) = THIRD*(wn4(2)+wa4(2)+wa3(2))
            wf4(3) = THIRD*(wn4(3)+wa4(3)+wa3(3))                                    
            !Grid velocity on face5 :
            wf5(1) = FOURTH*(wn4(1)+wn3(1)+wa5(1)+wa4(1))
            wf5(2) = FOURTH*(wn4(2)+wn3(2)+wa5(2)+wa4(2))
            wf5(3) = FOURTH*(wn4(3)+wn3(3)+wa5(3)+wa4(3))             
            !Grid velocity on face6 :
            wf6(1) = FOURTH*(wn3(1)+wn2(1)+wn1(1)+wa6(1)+wa5(1))
            wf6(2) = FOURTH*(wn3(2)+wn2(2)+wn1(2)+wa6(2)+wa5(2))
            wf6(3) = FOURTH*(wn3(3)+wn2(3)+wn1(3)+wa6(3)+wa5(3))                                                                      
            !Grid velocity on face0 :
            wf0(1) = ONE_OVER_6*(wa1(1)+wa2(1)+wa3(1)+wa4(1)+wa5(1)+wa6(1))
            wf0(2) = ONE_OVER_6*(wa1(2)+wa2(2)+wa3(2)+wa4(2)+wa5(2)+wa6(2))
            wf0(3) = ONE_OVER_6*(wa1(3)+wa2(3)+wa3(3)+wa4(3)+wa5(3)+wa6(3))             
            !storage
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%W(1:3)       = wf0(1:3)  
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%W(1:3) = wf1(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%W(1:3) = wf2(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%W(1:3) = wf3(1:3)                      
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(4))%W(1:3) = wf4(1:3)  
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(5))%W(1:3) = wf5(1:3)                 
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(6))%W(1:3) = wf6(1:3)             

          ELSEIF(SECTYPE(1:6)=='POLY4B')THEN
            nFACE   = F_POLY4B
            Fa(1:6) = Gface(1:6,SECID)
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,iPOSe(4))
            a5 => EDGE_LIST(NIN,IAD(5))%CUTPOINT(1:3,iPOSe(5))
            a6 => EDGE_LIST(NIN,IAD(6))%CUTPOINT(1:3,iPOSe(6))
            n1 => X(1:3,IXS(Gnode(1,SECID)+1,ID))
            n2 => X(1:3,IXS(Gnode(2,SECID)+1,ID))
            n3 => X(1:3,IXS(Gnode(3,SECID)+1,ID))
            n4 => X(1:3,IXS(Gnode(4,SECID)+1,ID))
            !interpolate w_a1
            beta    = EDGE_LIST(NIN,IAD(1))%CUTCOOR(iPOSe(1));
            J       = IAD(1)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa1(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa1(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa1(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a2
            beta    = EDGE_LIST(NIN,IAD(2))%CUTCOOR(iPOSe(2));
            J       = IAD(2)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa2(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa2(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa2(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a3
            beta    = EDGE_LIST(NIN,IAD(3))%CUTCOOR(iPOSe(3));
            J       = IAD(3)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa3(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa3(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa3(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a4
            beta    = EDGE_LIST(NIN,IAD(4))%CUTCOOR(iPOSe(4));
            J       = IAD(4)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa4(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa4(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa4(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a5
            beta    = EDGE_LIST(NIN,IAD(5))%CUTCOOR(iPOSe(5));
            J       = IAD(5)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa5(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa5(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa5(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a6
            beta    = EDGE_LIST(NIN,IAD(6))%CUTCOOR(iPOSe(6));
            J       = IAD(6)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa6(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa6(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa6(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !brick nodes                       
            wn1(1)= W(1,IXS(Gnode(1,SECID)+1,ID))
            wn1(2)= W(2,IXS(Gnode(1,SECID)+1,ID))
            wn1(3)= W(3,IXS(Gnode(1,SECID)+1,ID))
            !brick nodes                       
            wn2(1)= W(1,IXS(Gnode(2,SECID)+1,ID))
            wn2(2)= W(2,IXS(Gnode(2,SECID)+1,ID))
            wn2(3)= W(3,IXS(Gnode(2,SECID)+1,ID))   
            !brick nodes                       
            wn3(1)= W(1,IXS(Gnode(3,SECID)+1,ID))
            wn3(2)= W(2,IXS(Gnode(3,SECID)+1,ID))
            wn3(3)= W(3,IXS(Gnode(3,SECID)+1,ID))  
            !brick nodes                       
            wn4(1)= W(1,IXS(Gnode(4,SECID)+1,ID))
            wn4(2)= W(2,IXS(Gnode(4,SECID)+1,ID))
            wn4(3)= W(3,IXS(Gnode(4,SECID)+1,ID))                                                                       
            !Grid velocity on face1 :
            wf1(1) = THIRD*(wn1(1)+wa1(1)+wa6(1))
            wf1(2) = THIRD*(wn1(2)+wa1(2)+wa6(2))
            wf1(3) = THIRD*(wn1(3)+wa1(3)+wa6(3))                                                          
            !Grid velocity on face2 :
            wf2(1) = FOURTH*(wn1(1)+wn2(1)+wa2(1)+wa1(1))
            wf2(2) = FOURTH*(wn1(2)+wn2(2)+wa2(2)+wa1(2))
            wf2(3) = FOURTH*(wn1(3)+wn2(3)+wa2(3)+wa1(3))                                                                                
            !Grid velocity on face3 :
            wf3(1) = ONE_FIFTH*(wn2(1)+wn3(1)+wn4(1)+wa3(1)+wa2(1))
            wf3(2) = ONE_FIFTH*(wn2(2)+wn3(2)+wn4(2)+wa3(2)+wa2(2))
            wf3(3) = ONE_FIFTH*(wn2(3)+wn3(3)+wn4(3)+wa3(3)+wa2(3))                                                         
            !Grid velocity on face4 :
            wf4(1) = THIRD*(wn4(1)+wa4(1)+wa3(1))
            wf4(2) = THIRD*(wn4(2)+wa4(2)+wa3(2))
            wf4(3) = THIRD*(wn4(3)+wa4(3)+wa3(3))                                                
            !Grid velocity on face5 :
            wf5(1) = FOURTH*(wn4(1)+wn3(1)+wa5(1)+wa4(1))
            wf5(2) = FOURTH*(wn4(2)+wn3(2)+wa5(2)+wa4(2))
            wf5(3) = FOURTH*(wn4(3)+wn3(3)+wa5(3)+wa4(3))                         
            !Grid velocity on face6 :
            wf6(1) = FOURTH*(wn3(1)+wn2(1)+wn1(1)+wa6(1)+wa5(1))
            wf6(2) = FOURTH*(wn3(2)+wn2(2)+wn1(2)+wa6(2)+wa5(2))
            wf6(3) = FOURTH*(wn3(3)+wn2(3)+wn1(3)+wa6(3)+wa5(3))                                                                      
            !Grid velocity on face0 :
            wf0(1) = ONE_OVER_6*(wa1(1)+wa2(1)+wa3(1)+wa4(1)+wa5(1)+wa6(1))
            wf0(2) = ONE_OVER_6*(wa1(2)+wa2(2)+wa3(2)+wa4(2)+wa5(2)+wa6(2))
            wf0(3) = ONE_OVER_6*(wa1(3)+wa2(3)+wa3(3)+wa4(3)+wa5(3)+wa6(3))             
            !storage
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%W(1:3)       = wf0(1:3)  
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%W(1:3) = wf1(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%W(1:3) = wf2(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%W(1:3) = wf3(1:3)                      
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(4))%W(1:3) = wf4(1:3)  
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(5))%W(1:3) = wf5(1:3)                 
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(6))%W(1:3) = wf6(1:3)
                         
          ELSEIF(SECTYPE(1:5)=='POLYC')THEN
            nFACE   = F_POLYC     
            Fa(1:6) = Gface(1:6,SECID)
            a1 => EDGE_LIST(NIN,IAD(1))%CUTPOINT(1:3,iPOSe(1))
            a2 => EDGE_LIST(NIN,IAD(2))%CUTPOINT(1:3,iPOSe(2))
            a3 => EDGE_LIST(NIN,IAD(3))%CUTPOINT(1:3,iPOSe(3))
            a4 => EDGE_LIST(NIN,IAD(4))%CUTPOINT(1:3,iPOSe(4))
            a5 => EDGE_LIST(NIN,IAD(5))%CUTPOINT(1:3,iPOSe(5))  !points 5,6 on same edge : IAD(5)=IAD(6) BUT ICUT=1
            a6 => EDGE_LIST(NIN,IAD(6))%CUTPOINT(1:3,iPOSe(6))  !points 5,6 on same edge : IAD(5)=IAD(6) BUT ICUT=2
            n1 => X(1:3,IXS(Gnode(1,SECID)+1,ID))
            n2 => X(1:3,IXS(Gnode(2,SECID)+1,ID))
            n3 => X(1:3,IXS(Gnode(3,SECID)+1,ID))
            n4 => X(1:3,IXS(Gnode(4,SECID)+1,ID))
            !interpolate w_a1
            beta    = EDGE_LIST(NIN,IAD(1))%CUTCOOR(iPOSe(1));
            J       = IAD(1)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa1(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa1(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa1(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a2
            beta    = EDGE_LIST(NIN,IAD(2))%CUTCOOR(iPOSe(2));
            J       = IAD(2)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa2(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa2(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa2(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a3
            beta    = EDGE_LIST(NIN,IAD(3))%CUTCOOR(iPOSe(3));
            J       = IAD(3)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa3(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa3(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa3(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a4
            beta    = EDGE_LIST(NIN,IAD(4))%CUTCOOR(iPOSe(4));
            J       = IAD(4)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa4(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa4(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa4(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a5
            beta    = EDGE_LIST(NIN,IAD(5))%CUTCOOR(iPOSe(5));
            J       = IAD(5)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa5(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa5(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa5(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !interpolate w_a6
            beta    = EDGE_LIST(NIN,IAD(6))%CUTCOOR(iPOSe(6));
            J       = IAD(6)-(IB-1)*12;
            EdgNod1 = IXS(1+iEDGE(1,J),ID);
            EdgNod2 = IXS(1+iEDGE(2,J),ID)
            wa6(1)  = beta * W(1,EdgNod1) + (ONE-beta)*W(1,EdgNod2)
            wa6(2)  = beta * W(2,EdgNod1) + (ONE-beta)*W(2,EdgNod2)
            wa6(3)  = beta * W(3,EdgNod1) + (ONE-beta)*W(3,EdgNod2)
            !brick nodes                       
            wn1(1)= W(1,IXS(Gnode(1,SECID)+1,ID))
            wn1(2)= W(2,IXS(Gnode(1,SECID)+1,ID))
            wn1(3)= W(3,IXS(Gnode(1,SECID)+1,ID))
            !brick nodes                       
            wn2(1)= W(1,IXS(Gnode(2,SECID)+1,ID))
            wn2(2)= W(2,IXS(Gnode(2,SECID)+1,ID))
            wn2(3)= W(3,IXS(Gnode(2,SECID)+1,ID))   
            !brick nodes                       
            wn3(1)= W(1,IXS(Gnode(3,SECID)+1,ID))
            wn3(2)= W(2,IXS(Gnode(3,SECID)+1,ID))
            wn3(3)= W(3,IXS(Gnode(3,SECID)+1,ID))  
            !brick nodes                       
            wn4(1)= W(1,IXS(Gnode(4,SECID)+1,ID))
            wn4(2)= W(2,IXS(Gnode(4,SECID)+1,ID))
            wn4(3)= W(3,IXS(Gnode(4,SECID)+1,ID))                                                                                  
            !Grid velocity on face1 :
            wf1(1) = FOURTH*(wn1(1)+wn2(1)+wn3(1)+wn4(1))
            wf1(2) = FOURTH*(wn1(2)+wn2(2)+wn3(2)+wn4(2))
            wf1(3) = FOURTH*(wn1(3)+wn2(3)+wn3(3)+wn4(3))                                                                     
            !Grid velocity on face2 :
            wf2(1) = FOURTH*(wn1(1)+wa1(1)+wa2(1)+wn2(1))
            wf2(2) = FOURTH*(wn1(2)+wa1(2)+wa2(2)+wn2(2))
            wf2(3) = FOURTH*(wn1(3)+wa1(3)+wa2(3)+wn2(3))                                                                                           
            !Grid velocity on face3 :
            wf3(1) = FOURTH*(wn2(1)+wa2(1)+wa3(1)+wn3(1))
            wf3(2) = FOURTH*(wn2(2)+wa2(2)+wa3(2)+wn3(2))
            wf3(3) = FOURTH*(wn2(3)+wa2(3)+wa3(3)+wn3(3))                                                                   
            !Grid velocity on face4 :
            wf4(1) = FOURTH*(wn1(1)+wn4(1)+wa4(1)+wa1(1))
            wf4(2) = FOURTH*(wn1(2)+wn4(2)+wa4(2)+wa1(2))
            wf4(3) = FOURTH*(wn1(3)+wn4(3)+wa4(3)+wa1(3))                                                       
            !Grid velocity on face5 :
            wf5(1) = (wn4(1)+wn3(1)+wa3(1)+wa6(1)+wa5(1)+wa4(1)+wn4(1))/SEVEN
            wf5(2) = (wn4(2)+wn3(2)+wa3(2)+wa6(2)+wa5(2)+wa4(2)+wn4(2))/SEVEN
            wf5(3) = (wn4(3)+wn3(3)+wa3(3)+wa6(3)+wa5(3)+wa4(3)+wn4(3))/SEVEN                                                                        
            !Grid velocity on face0 :
            wf0(1) = ONE_OVER_6*(wa1(1)+wa2(1)+wa3(1)+wa4(1)+wa5(1)+wa6(1))
            wf0(2) = ONE_OVER_6*(wa1(2)+wa2(2)+wa3(2)+wa4(2)+wa5(2)+wa6(2))
            wf0(3) = ONE_OVER_6*(wa1(3)+wa2(3)+wa3(3)+wa4(3)+wa5(3)+wa6(3))             
            !storage
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE0%W(1:3)       = wf0(1:3)  
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(1))%W(1:3) = wf1(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(2))%W(1:3) = wf2(1:3)
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(3))%W(1:3) = wf3(1:3)                      
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(4))%W(1:3) = wf4(1:3)  
            BRICK_LIST(NIN,IB)%POLY(icell)%FACE(Fa(5))%W(1:3) = wf5(1:3)                 

          END IF
          ENDIF ! ILAE/=0






        END DO !ICELL


        NTAG = SUM(ITAG(1:8))
        IF(NTAG>0)THEN
          !CLEAN DEGENERATED TETRA & HEXA  (USELESS AND WOULD NEED TO BE ATTACHED TO A main CELL WHICH COULD NOT EXIST)
          NUMPT = 0
          DO K=1,8
            tmp_SECTYPE(K)    = BRICK_LIST(NIN,IB)%SECTYPE(K)
            tmp_SecID_CELL(K) = BRICK_LIST(NIN,IB)%SecID_CELL(K)
            !tmp_NumNOD(K)     = BRICK_LIST(NIN,IB)%POLY(K)%NumNOD
            !tmp_NumPOINT(K)   = BRICK_LIST(NIN,IB)%POLY(K)%NumPOINT
            tmpPOLY(K)        = BRICK_LIST(NIN,IB)%POLY(K)
            tmpCUT(K)         = BRICK_LIST(NIN,IB)%PCUT(K)
          ENDDO
          !polyhedron K deletion, updating identifiers (resorting) for polyhedrons ids K+1  (if K==8, nothing to do, last one deleted, nothing to resort)
          DO K=1,8
            IF(ITAG(K)==1)THEN
              DO N=K,7
                BRICK_LIST(NIN,IB)%SECTYPE(N)       = tmp_SECTYPE(N+1)    
                BRICK_LIST(NIN,IB)%SecID_CELL(N)    = tmp_SecID_CELL(N+1) 
                !BRICK_LIST(NIN,IB)%POLY(N)%NumNOD   = tmp_NumNOD(N+1)
                !BRICK_LIST(NIN,IB)%POLY(N)%NumPOINT = tmp_NumPOINT(N+1)          
                BRICK_LIST(NIN,IB)%POLY(N)          = tmpPOLY(N+1)
                BRICK_LIST(NIN,IB)%PCUT(N)          = tmpCUT(N+1)
              ENDDO
              NUMPT=NUMPT+1
              !updating buffer whichcellnode. find a cell_id from a local node_id. This changed due to polyhedra k deletion and resorting
              DO N=1,8
                INOD = BRICK_LIST(NIN,IB)%NODE(N)%WhichCell
                IF(INOD >= K)THEN
                  BRICK_LIST(NIN,IB)%NODE(N)%WhichCell = INOD - NTAG
                ENDIF
              ENDDO
            ENDIF
          ENDDO
          BRICK_LIST(NIN,IB)%NBCUT = BRICK_LIST(NIN,IB)%NBCUT - NUMPT        
          DO K=BRICK_LIST(NIN,IB)%NBCUT+1,8
            BRICK_LIST(NIN,IB)%PCUT(K)%NP        = 0
            BRICK_LIST(NIN,IB)%POLY(K)%NumNOD    = 0
            BRICK_LIST(NIN,IB)%POLY(K)%NumPOINT  = 0
            BRICK_LIST(NIN,IB)%SecID_CELL(K)     = 0
            BRICK_LIST(NIN,IB)%SECTYPE(K)        = '______________'   
          ENDDO
        ENDIF !(NTAG>0)
       
        !----------------------------------------------------------------
        !   Removing Unused Intersection Point. 
        !    (Otherwise troube with complementary cell #9 in sinit22.F)
        !----------------------------------------------------------------
        DO J=1,12
          K = (IB-1)*12+J  
          !EdgeList(J)     : real number of points used on edge J      
           !EdgePos(J,1:2) : their positiono n edge 1,or 2, or 1&2
          IF(EdgeList(J) /= EDGE_LIST(NIN,K)%NBCUT)THEN
            IF(EdgeList(J)==1)THEN
              EDGE_LIST(NIN,K)%NBCUT = 1
              !keep only relevant node
              IF(EdgePos(J,1)==1)THEN
                !do nothing, keep first one
              ELSEIF(EdgePos(J,2)==1)THEN
                !switch
                EDGE_LIST(NIN,K)%CUTSHELL(1)     = EDGE_LIST(NIN,K)%CUTSHELL(2)  
                EDGE_LIST(NIN,K)%CUTCOOR(1)      = EDGE_LIST(NIN,K)%CUTCOOR(2)              
                EDGE_LIST(NIN,K)%CUTPOINT(1:3,1) = EDGE_LIST(NIN,K)%CUTPOINT(1:3,2)
                EDGE_LIST(NIN,K)%CUTVEL(1:3,1)   = EDGE_LIST(NIN,K)%CUTVEL(1:3,2)       
              ENDIF
            ELSEIF(EdgeList(J)==0)THEN
              EDGE_LIST(NIN,K)%NBCUT = 0
            ENDIF
          ENDIF
        ENDDO  
            
        
        !----------------------------------------------------------------
        !   Build Cell 9 which is complementary one.
        !----------------------------------------------------------------
        ! moved to sinit22.F        


      IF(BRICK_LIST(NIN,IB)%NBCUT == 0)THEN
        ICELL              = 1
        pNNodCell(ICELL)%NumNOD   = 8  !main(brick nodes)
        pNPointCell(ICELL)%NumPOINT = 8 !main + intersection
        n1 => X(1:3, IXS(1+1,BRICK_LIST(NIN,IB)%ID ))
        n2 => X(1:3, IXS(1+2,BRICK_LIST(NIN,IB)%ID ))
        n3 => X(1:3, IXS(1+3,BRICK_LIST(NIN,IB)%ID ))
        n4 => X(1:3, IXS(1+4,BRICK_LIST(NIN,IB)%ID ))
        n5 => X(1:3, IXS(1+5,BRICK_LIST(NIN,IB)%ID ))
        n6 => X(1:3, IXS(1+6,BRICK_LIST(NIN,IB)%ID ))
        n7 => X(1:3, IXS(1+7,BRICK_LIST(NIN,IB)%ID ))
        n8 => X(1:3, IXS(1+8,BRICK_LIST(NIN,IB)%ID ))                                                        
        BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(1)= SUM( (/n1(1),n2(1),n3(1),n4(1),n5(1),n6(1),n7(1),n8(1)/) ) / EIGHT
        BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(2)= SUM( (/n1(2),n2(2),n3(2),n4(2),n5(2),n6(2),n7(2),n8(2)/) ) / EIGHT
        BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCenter(3)= SUM( (/n1(3),n2(3),n3(3),n4(3),n5(3),n6(3),n7(3),n8(3)/) ) / EIGHT
        BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:8) = (/ 1,2,3,4,5,6,7,8/)
        BRICK_LIST(NIN,IB)%NODE(1:8)%WhichCell = ICELL
      ENDIF

      END DO !next IB (cut cell)


      !----------------------------------------------------------------
      !   double Multiplicity Intersection.
      !----------------------------------------------------------------
      DO IB=NBF,NBL !1,NB
        IF(BRICK_LIST(NIN,IB)%NBCUT == 2)THEN
          SECTYPE  = BRICK_LIST(NIN,IB)%SECTYPE(1)                                                                                 
          SECTYPE2 = BRICK_LISt(NIN,IB)%SECTYPE(2)                                                                                 
          Cod1    = IABS(BRICK_LIST(NIN,IB)%SecID_CELL(1))                                                                         
          Cod2    = IABS(BRICK_LIST(NIN,IB)%SecID_CELL(2))                                                                         
          IF(Cod1==Cod2)THEN                                                                                                       
            Vol1  = BRICK_LIST(NIN,IB)%POLY(1)%Vnew                                                                                 
            Vol2  = BRICK_LIST(NIN,IB)%POLY(2)%Vnew                                                                                                                                                                                                                                                                                                                                   
            NUMPT = BRICK_LIST(NIN,IB)%POLY(1)%NumPOINT 
                                                                                                                                               
            IF(Vol1>Vol2)THEN                                                                                                   
              
              ValTMP(1:3)                                = BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(1:3)
                                                                                                                                  
              !bakup poly1                                                                                                         
              BRICK_LIST(NIN,IB)%POLY(3)%FACE(1:6)%Surf  = BRICK_LIST(NIN,IB)%POLY(1)%FACE(1:6)%Surf                                     
              BRICK_LIST(NIN,IB)%PCUT(3)%SCUT            = BRICK_LIST(NIN,IB)%PCUT(1)%SCUT                                         
              BRICK_LIST(NIN,IB)%PCUT(3)%B(1:3)          = BRICK_LIST(NIN,IB)%PCUT(1)%B(1:3)                                       
              BRICK_LIST(NIN,IB)%PCUT(3)%N(1:3)          = BRICK_LIST(NIN,IB)%PCUT(1)%N(1:3)                                       
              BRICK_LIST(NIN,IB)%PCUT(3)%NP              = BRICK_LIST(NIN,IB)%PCUT(1)%NP                                           
              BRICK_LIST(NIN,IB)%PCUT(3)%P(1:3,1)        = BRICK_LIST(NIN,IB)%PCUT(1)%P(1:3,1)                                     
              BRICK_LIST(NIN,IB)%PCUT(3)%P(1:3,2)        = BRICK_LIST(NIN,IB)%PCUT(1)%P(1:3,2)                                     
              BRICK_LIST(NIN,IB)%PCUT(3)%P(1:3,3)        = BRICK_LIST(NIN,IB)%PCUT(1)%P(1:3,3)                                     
              BRICK_LIST(NIN,IB)%PCUT(3)%P(1:3,4)        = BRICK_LIST(NIN,IB)%PCUT(1)%P(1:3,4)                                     
              BRICK_LIST(NIN,IB)%PCUT(3)%P(1:3,5)        = BRICK_LIST(NIN,IB)%PCUT(1)%P(1:3,5)                                     
              BRICK_LIST(NIN,IB)%PCUT(3)%P(1:3,6)        = BRICK_LIST(NIN,IB)%PCUT(1)%P(1:3,6)                                     
              BRICK_LIST(NIN,IB)%POLY(3)%Vnew            = BRICK_LIST(NIN,IB)%POLY(1)%Vnew                                         
              BRICK_LIST(NIN,IB)%POLY(3)%NumNOD          = BRICK_LIST(NIN,IB)%POLY(1)%NumNOD                                       
              BRICK_LIST(NIN,IB)%POLY(3)%NumPOINT        = BRICK_LIST(NIN,IB)%POLY(1)%NumPOINT                                                                            
              BRICK_LIST(NIN,IB)%POLY(3)%ListNodID(1:8)  = BRICK_LIST(NIN,IB)%POLY(1)%ListNodID(1:8)                                        
              BRICK_LIST(NIN,IB)%PCUT(3)%Vel(1:3)        = BRICK_LIST(NIN,IB)%PCUT(1)%Vel(1:3)                                        

              !switch poly2 into poly1                                                                                             
              !1. poly1 <- poly2                                                                                                   
              BRICK_LIST(NIN,IB)%POLY(1)%FACE(1:6)%Surf  = BRICK_LIST(NIN,IB)%POLY(2)%FACE(1:6)%Surf                                      
              BRICK_LIST(NIN,IB)%PCUT(1)%SCUT            = BRICK_LIST(NIN,IB)%PCUT(2)%SCUT                                          
              BRICK_LIST(NIN,IB)%PCUT(1)%B(1:3)          = BRICK_LIST(NIN,IB)%PCUT(2)%B(1:3)                                        
              BRICK_LIST(NIN,IB)%PCUT(1)%N(1:3)          = BRICK_LIST(NIN,IB)%PCUT(2)%N(1:3)                                        
              BRICK_LIST(NIN,IB)%PCUT(1)%NP              = BRICK_LIST(NIN,IB)%PCUT(2)%NP                                            
              BRICK_LIST(NIN,IB)%PCUT(1)%P(1:3,1)        = BRICK_LIST(NIN,IB)%PCUT(2)%P(1:3,1)                                      
              BRICK_LIST(NIN,IB)%PCUT(1)%P(1:3,2)        = BRICK_LIST(NIN,IB)%PCUT(2)%P(1:3,2)                                      
              BRICK_LIST(NIN,IB)%PCUT(1)%P(1:3,3)        = BRICK_LIST(NIN,IB)%PCUT(2)%P(1:3,3)                                      
              BRICK_LIST(NIN,IB)%PCUT(1)%P(1:3,4)        = BRICK_LIST(NIN,IB)%PCUT(2)%P(1:3,4)                                      
              BRICK_LIST(NIN,IB)%PCUT(1)%P(1:3,5)        = BRICK_LIST(NIN,IB)%PCUT(2)%P(1:3,5)                                      
              BRICK_LIST(NIN,IB)%PCUT(1)%P(1:3,6)        = BRICK_LIST(NIN,IB)%PCUT(2)%P(1:3,6)                                      
              BRICK_LIST(NIN,IB)%POLY(1)%Vnew            = BRICK_LIST(NIN,IB)%POLY(2)%Vnew                                          
              BRICK_LIST(NIN,IB)%POLY(1)%NumNOD          = BRICK_LIST(NIN,IB)%POLY(2)%NumNOD                                        
              BRICK_LIST(NIN,IB)%POLY(1)%NumPOINT        = BRICK_LIST(NIN,IB)%POLY(2)%NumPOINT                                      
              BRICK_LIST(NIN,IB)%POLY(1)%ListNodID(1:8)  = BRICK_LIST(NIN,IB)%POLY(2)%ListNodID(1:8)                                      
              BRICK_LIST(NIN,IB)%PCUT(1)%Vel(1:3)        = BRICK_LIST(NIN,IB)%PCUT(2)%Vel(1:3)                                     

              !2. poly2 <- poly3                                                                                                   
              BRICK_LIST(NIN,IB)%POLY(2)%FACE(1:6)%Surf  = -BRICK_LIST(NIN,IB)%POLY(3)%FACE(1:6)%Surf                                      
              BRICK_LIST(NIN,IB)%PCUT(2)%SCUT            = BRICK_LIST(NIN,IB)%PCUT(3)%SCUT                                             
              BRICK_LIST(NIN,IB)%PCUT(2)%B(1:3)          = BRICK_LIST(NIN,IB)%PCUT(3)%B(1:3)                                           
              BRICK_LIST(NIN,IB)%PCUT(2)%N(1:3)          = BRICK_LIST(NIN,IB)%PCUT(3)%N(1:3)                                           
              BRICK_LIST(NIN,IB)%PCUT(2)%NP              = BRICK_LIST(NIN,IB)%PCUT(3)%NP                                               
              BRICK_LIST(NIN,IB)%PCUT(2)%P(1:3,1)        = BRICK_LIST(NIN,IB)%PCUT(3)%P(1:3,1)                                         
              BRICK_LIST(NIN,IB)%PCUT(2)%P(1:3,2)        = BRICK_LIST(NIN,IB)%PCUT(3)%P(1:3,2)                                         
              BRICK_LIST(NIN,IB)%PCUT(2)%P(1:3,3)        = BRICK_LIST(NIN,IB)%PCUT(3)%P(1:3,3)                                         
              BRICK_LIST(NIN,IB)%PCUT(2)%P(1:3,4)        = BRICK_LIST(NIN,IB)%PCUT(3)%P(1:3,4)                                         
              BRICK_LIST(NIN,IB)%PCUT(2)%P(1:3,5)        = BRICK_LIST(NIN,IB)%PCUT(3)%P(1:3,5)                                         
              BRICK_LIST(NIN,IB)%PCUT(2)%P(1:3,6)        = BRICK_LIST(NIN,IB)%PCUT(3)%P(1:3,6)                                         
              BRICK_LIST(NIN,IB)%POLY(2)%Vnew            = -BRICK_LIST(NIN,IB)%POLY(3)%Vnew                                            
              BRICK_LIST(NIN,IB)%POLY(2)%NumNOD          = 8-BRICK_LIST(NIN,IB)%POLY(3)%NumNOD
              BRICK_LIST(NIN,IB)%POLY(2)%NumPOINT        = BRICK_LIST(NIN,IB)%POLY(1)%NumPOINT-BRICK_LIST(NIN,IB)%POLY(1)%NumNOD       
     .                                                     + 8-BRICK_LIST(NIN,IB)%POLY(3)%NumNOD                                       
              BRICK_LIST(NIN,IB)%POLY(2)%ListNodID(1:8)  = BRICK_LIST(NIN,IB)%POLY(3)%ListNodID(1:8)                                        
              BRICK_LIST(NIN,IB)%PCUT(2)%Vel(1:3)        = BRICK_LIST(NIN,IB)%PCUT(3)%Vel(1:3)                                        

              !3.reset poly3.                                                                                                      
              BRICK_LIST(NIN,IB)%POLY(3)%FACE(1:6)%Surf     = ZERO                                                                 
              BRICK_LIST(NIN,IB)%POLY(3)%Vnew               = ZERO                                                                 
              BRICK_LIST(NIN,IB)%POLY(3)%NumNOD             = 0                                                                    
              BRICK_LIST(NIN,IB)%POLY(3)%NumPOINT           = 0                                                                    
              BRICK_LIST(NIN,IB)%POLY(3)%CellCenter(1)      = ZERO                                                                 
              BRICK_LIST(NIN,IB)%POLY(3)%CellCenter(2)      = ZERO                                                                 
              BRICK_LIST(NIN,IB)%POLY(3)%CellCenter(3)      = ZERO                                                                 
              BRICK_LIST(NIN,IB)%POLY(3)%ListNodID(1:8)     = 0 
              DO K=0,6                                                                      
              BRICK_LIST(NIN,IB)%POLY(3)%FACE(K)%Vel(1)     = ZERO 
              BRICK_LIST(NIN,IB)%POLY(3)%FACE(K)%Vel(2)     = ZERO 
              BRICK_LIST(NIN,IB)%POLY(3)%FACE(K)%Vel(3)     = ZERO 
              ENDDO
              
              DO I=1,8                                                                                                               
                IF(BRICK_LIST(NIN,IB)%NODE(I)%WhichCell == 2)THEN   
                                                                                
                ELSE
                  BRICK_LIST(NIN,IB)%NODE(I)%WhichCell = 1                                                                                         
                ENDIF                                                                                                                
              ENDDO      
              
              !cell center update
              !from previous calculation based on intersection points + nodes, remove nodes and add new ones.
              BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(1:3) =  BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(1:3) * NUMPT   !(somme directe, et non plus une moyenne)
              DO I=1,8   
                nn => X(1:3, IXS(1+I,BRICK_LIST(NIN,IB)%ID ))                                                                                                                          
                IF(BRICK_LIST(NIN,IB)%NODE(I)%WhichCell == 2)THEN                                                                  
                  BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(1) =  BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(1) - nn(1)
                  BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(2) =  BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(2) - nn(2)
                  BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(3) =  BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(3) - nn(3) 
                  NUMPT = NUMPT - 1
                 ELSE   
                  BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(1) =  BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(1) + nn(1)
                  BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(2) =  BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(2) + nn(2)
                  BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(3) =  BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(3) + nn(3)
                  NUMPT = NUMPT + 1                                                                    
                ENDIF                                                                                                                
              ENDDO  
              BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(1:3) =  BRICK_LIST(NIN,IB)%POLY(1)%CellCenter(1:3) / NUMPT   !on retrouve une moyenne


                                                                                                                                   
            !IF(Vol1>Vol2)THEN                                                                                                  
            ELSE 
              ValTMP(1:3)                               = BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(1:3)  
              BRICK_LIST(NIN,IB)%POLY(2)%FACE(1:6)%Surf = -BRICK_LIST(NIN,IB)%POLY(2)%FACE(1:6)%Surf                                                                                                                                                                                  
              BRICK_LIST(NIN,IB)%POLY(2)%Vnew           = -BRICK_LIST(NIN,IB)%POLY(2)%Vnew                                            
              BRICK_LIST(NIN,IB)%POLY(2)%NumNOD         = 8-BRICK_LIST(NIN,IB)%POLY(2)%NumNOD                                         
              BRICK_LIST(NIN,IB)%POLY(2)%NumPOINT       = BRICK_LIST(NIN,IB)%POLY(2)%NumPOINT-BRICK_LIST(NIN,IB)%POLY(1)%NumNOD       
     .                                                  + 8-BRICK_LIST(NIN,IB)%POLY(1)%NumNOD     
              DO I=1,8                                                                                                               
                IF(BRICK_LIST(NIN,IB)%NODE(I)%WhichCell == 2)THEN                                                                  
                  BRICK_LIST(NIN,IB)%NODE(I)%WhichCell = 1  
                ELSE
                  BRICK_LIST(NIN,IB)%NODE(I)%WhichCell = 2                                                                                         
                ENDIF                                                                                                                
              ENDDO  

              !cell center update
              !from previous calculation based on intersection points + nodes, remove nodes and add new ones.
              NUMPT = BRICK_LIST(NIN,IB)%POLY(2)%NumPOINT                                                                                  
              BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(1:3) =  BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(1:3) * NUMPT   !(somme directe, et non plus une moyenne)
              DO I=1,8                                                                                                               
                IF(BRICK_LIST(NIN,IB)%NODE(I)%WhichCell == 1)THEN                                                                  
                  nn => X(1:3, IXS(1+I,BRICK_LIST(NIN,IB)%ID ))
                  BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(1) =  BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(1) - nn(1)
                  BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(2) =  BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(2) - nn(2)
                  BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(3) =  BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(3) - nn(3) 
                  NUMPT = NUMPT - 1
                 ELSE   
                  nn => X(1:3, IXS(1+I,BRICK_LIST(NIN,IB)%ID ))
                  BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(1) =  BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(1) + nn(1)
                  BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(2) =  BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(2) + nn(2)
                  BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(3) =  BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(3) + nn(3)
                  NUMPT = NUMPT + 1                                                                    
                ENDIF                                                                                                                
              ENDDO  
              BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(1:3) =  BRICK_LIST(NIN,IB)%POLY(2)%CellCenter(1:3) / NUMPT   !on retrouve une moyenne              
                                              
            ENDIF !Vol1>Vol2                                                                                                                 
                                                                                                                                   
                                                                                                                                   
            !set new list nod (complementary from poly 1)                                                                          
            INodTAG(1:8) = 0                                                                                                       
            DO I=1,BRICK_LIST(NIN,IB)%POLY(1)%NumNOD                                                                               
              INodTAG( BRICK_LIST(NIN,IB)%POLY(1)%ListNodID(I) ) = 1                                                                     
            ENDDO                                                                                                                  
            J = 1                                                                                                                  
            DO I=1,8                                                                                                               
              IF(INodTAG(I) == 0)THEN                                                                                              
                BRICK_LIST(NIN,IB)%POLY(2)%ListNodID(J)    = I                                                                              
                J = J + 1                                                                                                          
              ENDIF                                                                                                                
            ENDDO  
            
            BRICK_LIST(NIN,IB)%POLY(9)%CellCenter(1:3) = ValTMP(1:3)
            BRICK_LIST(NIN,IB)%POLY(9)%NumNOD          = 0
            BRICK_LIST(NIN,IB)%POLY(9)%NumPOINT        = 0
                                                                                                                                   
          ENDIF   !(Cod1==Cod2)                                                                                                                 
        ENDIF !IF(BRICK_LIST(NIN,IB)%NBCUT == 2)
      ENDDO !next IB        


      !----------------------------------------------------------------
      !  save edge data in cut cell buffer for velocity interpolation.
      !---------------------------------------------------------------- 
      DO IB=NBF,NBL
        NBCUT = BRICK_LIST(NIN,IB)%NBCUT
        !IF(NBCUT==0)CYCLE !need EDGE_LIST(NIN,K)%NBCUT for velocity interpolation
        DO J=1,12
          K                                          = (IB-1)*12+J
          BRICK_LIST(NIN,IB)%Edge(J)%NODE(1:2)       = EDGE_LIST(NIN,K)%NODE(1:2)
          BRICK_LIST(NIN,IB)%Edge(J)%NBCUT           = EDGE_LIST(NIN,K)%NBCUT
          BRICK_LIST(NIN,IB)%Edge(J)%CUTSHELL(1:2)   = EDGE_LIST(NIN,K)%CUTSHELL(1:2)
          BRICK_LIST(NIN,IB)%Edge(J)%CUTCOOR(1:2)    = EDGE_LIST(NIN,K)%CUTCOOR(1:2)                   
          BRICK_LIST(NIN,IB)%Edge(J)%CUTPOINT(1:3,1) = EDGE_LIST(NIN,K)%CUTPOINT(1:3,1)
          BRICK_LIST(NIN,IB)%Edge(J)%CUTPOINT(1:3,2) = EDGE_LIST(NIN,K)%CUTPOINT(1:3,2)
          BRICK_LIST(NIN,IB)%Edge(J)%CUTVEL(1:3,1)   = EDGE_LIST(NIN,K)%CUTVEL(1:3,1)
          BRICK_LIST(NIN,IB)%Edge(J)%CUTVEL(1:3,2)   = EDGE_LIST(NIN,K)%CUTVEL(1:3,2) 
          BRICK_LIST(NIN,IB)%Edge(J)%VECTOR(1:3)     = EDGE_LIST(NIN,K)%VECTOR(1:3)                             
          BRICK_LIST(NIN,IB)%Edge(J)%LEN             = EDGE_LIST(NIN,K)%LEN           
        ENDDO!next J
      ENDDO!next IB


      !----------------------------------------------------------------
      !  3. ANIMATION (reset others)
      !----------------------------------------------------------------
      CALL MY_BARRIER
      
      IF (II<=NEL.AND.NSPMD<=1.AND.ITASK==0) THEN
#include "lockon.inc"
            !initialisation des noeuds du grsh3n a ZERO.
            DO J=1,NEL
              !EXIT
c              print *, "idsh3n=", IXTG(NIXTG,IBUFSSG(J))  !IDSPRI=
              X(1:3,IXTG(2,IGRSH3N(IGR)%ENTITY(J)))=(/ZERO,ZERO,ZERO/)
              X(1:3,IXTG(3,IGRSH3N(IGR)%ENTITY(J)))=(/ZERO,ZERO,ZERO/)
              X(1:3,IXTG(4,IGRSH3N(IGR)%ENTITY(J)))=(/ZERO,ZERO,ZERO/)              
              V(1:3,IXTG(2,IGRSH3N(IGR)%ENTITY(J)))=(/ZERO,ZERO,ZERO/)
              V(1:3,IXTG(3,IGRSH3N(IGR)%ENTITY(J)))=(/ZERO,ZERO,ZERO/)
              V(1:3,IXTG(4,IGRSH3N(IGR)%ENTITY(J)))=(/ZERO,ZERO,ZERO/)
            END DO  
#include "lockoff.inc"
      END IF
      !---------------------------------------------------------------- 



        !----------------------------------------------------------------                                          
        !debug:  
        if(itask==0)then
               
        if (IBUG22_subvol==1)then
        do ib=1,NB 
          ID = brick_list(nin,ib)%ID
          NCELL = brick_list(nin,ib)%NBCUT 
            write (*,FMT='(A,2I12)') "=== BRICK ID===",  IXS(11,ID) , ID  
            icell = 1          
            do while (icell <= ncell .OR. icell == 9 ) !loop on ICELL
              mnod = BRICK_LIST(NIN,IB)%POLY(ICELL)%NumNOD 
                write (*,FMT='(A )')    "|"
                if(icell==9)then
                write (*,FMT='(A,I1,A,A  ,A1,I2,A1 )') "+== ICELL= ", ICELL , ", SecType=", 'COMPLEMENT' ,
     .                                                                       "(",  0  , ")"               
                else                                                                                            
                write (*,FMT='(A,I1,A,A14,A1,I2,A1 )') "+== ICELL= ", ICELL , ", SecType=", BRICK_LIST(NIN,IB)%SECTYPE(ICELL) ,
     .                                                                       "(",  BRICK_LIST(NIN,IB)%SecID_Cell(ICELL)  , ")"  
                endif
                pFACE(1:6) => BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)!%Surf
                pFACE0=> BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Surf
           write (*,FMT='(A )')           "|  |"                                                                               
           write (*,FMT='(A,F20.14)')     "|  +======SUVOLUMES=",  BRICK_LIST(NIN,IB)%POLY(ICELL)%Vnew                                                                 
           write (*,FMT='(A,6F20.14)')    "|  +=======SUBFACES=",  BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Surf                                        
           write (*,FMT='(A,F20.14)')     "|  +=======CUT AERA=",  BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Surf                                                                  
           write (*,FMT='(A,A,I2)')       "|  +======NUM POINT=","  ",BRICK_LIST(NIN,IB)%POLY(ICELL)%NumPOINT
           write (*,FMT='(A,A,I1,A,8I12)')"|  +======NODE LIST=","  (",MNOD,") ", BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:MNOD)
           write (*,FMT='(A,A ,8I12)')    "|            radIDs=", "      ", 
     .                                                            IXS(1+BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:MNOD),ID)
           write (*,FMT='(A,A ,8I12)')    "|           userIDs=","      ", 
     .                                                           ITAB(IXS(1+BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:MNOD),ID))
!                  do i=0,6                                                                                              
!                    print *, "Sn=", vF(:,I)                                                                             
!                    print *, " C=", C(1:3, Fa(I))                                                                       
!                  end do                                                                                                
               icell = icell + 1                                                                                                  
               if(icell>ncell .and. icell/=10) icell = 9   ! list is also : {1:ncell} U {9}
            enddo!next icell
            write (*,FMT='(A )')    " "             
          
        end do!next I
        
        endif!(IBUG22_subvol==1)
        end if
        !----------------------------------------------------------------     


      RETURN
      END


      FUNCTION I22AERA(NPTS, P, C)
C=======================================================================     
C-----------------------------------------------
C  D e s c r i p t i o n
C-----------------------------------------------
C Calcul les aires orientees des polygones P0, P1, .., Pn  avec n=NPTS
C Precondition : NPTS>=3, &  P0, P1, P3 non colineaires
C OPTIMISATION POUR LA SUITE : ne pas calculer les centroides ici car on duplique certaines coperations.
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real                      :: I22AERA(3)
C-----------------------------------------------
      INTEGER, INTENT(IN)          :: NPTS
      my_real, INTENT(IN)          :: P(3,NPTS)     
      my_real, INTENT(INOUT)       :: C(3)  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER H, K,L,R,I,Imax
      my_real
     .        Hh, DIAG13(3), DIAG24(3), S2n(3), NormL2, S2,
     .        V1(1:3), V2(1:3), V3(1:3), V4(1:3)
C-----------------------------------------------

      IF(NPTS<=2)THEN
        !print *, "**error in inter22 : contact force algorithm"
        I22AERA = ZERO
        stop 222
        return     
      ELSEIF (NPTS<=4)THEN         
        !produit vectoriel des diagonales diag(13) x diag(24)
        Imax         = MAX(NPTS,3)
        DIAG13(1:3)  = P(1:3,3) - P(1:3,1)
        DIAG24(1:3)  = P(1:3,Imax) - P(1:3,2)     
        S2n(1)       = DIAG13(2)*DIAG24(3) - DIAG13(3)*DIAG24(2)
        S2n(2)       = DIAG13(3)*DIAG24(1) - DIAG13(1)*DIAG24(3)
        S2n(3)       = DIAG13(1)*DIAG24(2) - DIAG13(2)*DIAG24(1)
        S2n(1:3)     = HALF * S2n(1:3)
        I22AERA(1:3) = S2n(1:3)
        !I22AERA =  SQRT(SUM(S2n(1:3)*S2n(1:3)))
      ELSEIF (NPTS>=5) THEN
         K=NPTS
         Hh=HALF*(K-1)
         H=INT(Hh)
         IF( MOD(K,2) == 1 )THEN
           L=0
         ELSE
           L=K-1
         END IF
         DO I=1, H-1
           V1(1:3)  = P(1:3,  2*I+1)  - P(1:3,   1)
           V2(1:3)  = P(1:3,2*(I+1))  - P(1:3, 2*I)         
           V3(1:3)  = P(1:3,  2*H+1)  - P(1:3,   1)
           V4(1:3)  = P(1:3,    L+1)  - P(1:3, 2*H)
           S2n(1)   = V1(2)*V2(3) - V1(3)*V2(2)
           S2n(2)   = V1(3)*V2(1) - V1(1)*V2(3)
           S2n(3)   = V1(1)*V2(2) - V1(2)*V2(1)                     
           S2n(1)   = V3(2)*V4(3) - V3(3)*V4(2) + S2n(1)
           S2n(2)   = V3(3)*V4(1) - V3(1)*V4(3) + S2n(2)
           S2n(3)   = V3(1)*V4(2) - V3(2)*V4(1) + S2n(3)                   
           S2n(1:3) = HALF*S2n(1:3)
           I22AERA  = S2n(1:3)
           !I22AERA = SQRT(SUM(S2n(1:3)*S2n(1:3)))
         END DO
      END IF

      C(1:3)=P(1:3,1)
      DO I=2,NPTS
        C(1:3)=C(1:3)+P(1:3,I)
      END DO
      C(1:3)=C(1:3)/NPTS

      RETURN
      END

