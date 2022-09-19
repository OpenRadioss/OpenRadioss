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
Chd|  DESTROY_CELL                  source/interfaces/int22/destroy_cell.F
Chd|-- called by -----------
Chd|        SINIT22_FVM                   source/interfaces/int22/sinit22_fvm.F
Chd|-- calls ---------------
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE DESTROY_CELL(NIN,IB,ICELL_TARGET,ICELLv,IBv,J,Jv, IXS, ITASK)
C-----------------------------------------------
C   D e s c r i p t i o n
C----------------------------------------------- 
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandonned, and is not an official option.
C
C Destroy a cell ICELL_target for a given cut 
C cell IB in the cut cell buffer.
C When a Cell is destroyed from the list , Cell
C list is updated taken into account : Volume, 
C Faces, and Ajdacent Cell data
C What needs to be update in cut cell buffer :
C      TYPE BRICK_ENTITY 
C        INTEGER                         :: NBCUT                   !nombre de plans de coupe
C        INTEGER                         :: SECID_Cell(8)           !cut local id in [1,8] -> [1,52] = sec type. Each possible cut is numbered from 1 to 52 ; 8 nodes => 8 possible cut cells
C        INTEGER                         :: WhichCell_Node(8)       !node local id in[1,8] -> [1,8] = local cut cell id.
C        INTEGER                         :: %POLY()%NumNOD(9)          !noeuds sommets
C        INTEGER                         :: %POLY()%NumPOINT        !nombre total de point decrivant le volume
C        INTEGER                         :: %POLY()%FACE()%NumPOINT      !gives number of points (node+intersec.pts) on a given cell face        
C        INTEGER                         :: Adjacent_Cell(6,9,5)    !index3: IADJ (adjacent cell id 1:%NAdjCell)
C        INTEGER                         :: NAdjCell(9,6)           !Number of adjacent cell for a given cell 1:9 and a given face 1:6
C        INTEGER                         :: %POLY()%ListNodID(8)          !Global nodes ID (max8) describing the cell, for each of 9 cells
C        my_real                         :: %POLY()%Vnew            !volume des sous volumes elementaires
C        my_real                         :: %POLY()%FACE()%Surf     !face0=intersection surface
C        TYPE(CUT_PLANE)                 :: PCUT(8)
C        CHARACTER*14                    :: SECTYPE(8)
C      END TYPE BRICK_ENTITY
C Other quantities are not affected yet or they
C are related to nodal values.
C TO DO LATER : Optimize data derived type by
C example : %CELL(1) = %CELL(9) (only 1 line)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE I22BUFBRIC_MOD
      USE I22TRI_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN)  :: IB,ICELL_TARGET,ICELLv,IBv,J,Jv, IXS(NIXS,*),NIN,ITASK
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER             :: NCELL,ICELL, NAdjv,IADJv,ID,NBCUT,ICELL_DEST,IADJ, NListNod9,NADj, IE, K
      INTEGER             :: WhichCell_Node(8),INOD, I, NumNODES_Cell, NumIntP,NumPOINT_Face(6),ListNodID(8)
      my_real             :: FACE_CELL(1:6)
      TYPE (BRICK_ENTITY) :: CUTCELL_TMP
      LOGICAL             ::  bFOUND
C-----------------------------------------------
C   S o u r c e    L i n e s
C-----------------------------------------------      
        ID          = BRICK_LIST(NIN,IB)%ID
        NBCUT       = BRICK_LIST(NIN,IB)%NBCUT
        NCELL       = NBCUT

        if(itask==0)then
        if(ibug22_destroy==-1 .or. ibug22_destroy==ixs(11,brick_list(nin,ib)%id))then
          ie = brick_list(nin,ib)%id
          write (*,FMT='(A,I10,A1,I1)') "destroying cell", ixs(11,ie),".",ICELL_TARGET
        endif
        endif

        IF(NCELL==0 .OR. (NBCUT>1.AND.ICELL_TARGET==9) )THEN
          print *, "**error inter22 : unexpected situation, elem id=", IXS(11,ID)
        ENDIF

        IF(ICELL_TARGET==9)THEN
          !--------------------------------------------------------------------------------!
          !  SIMPLY DESTROY CELL 9                                                         !
          !--------------------------------------------------------------------------------!

          !---ERASE DATA FROM CELL 9
          BRICK_LIST(NIN,IB)%NBCUT                    = 0
          BRICK_LIST(NIN,IB)%SECID_Cell(1)            = 0

          BRICK_LIST(NIN,IB)%NODE(1:8)%WhichCell      = 1

          BRICK_LIST(NIN,IB)%POLY(1)%NumNOD           = 8
          BRICK_LIST(NIN,IB)%POLY(9)%NumNOD          = 0

          BRICK_LIST(NIN,IB)%POLY(1)%NumPOINT         = 8
          BRICK_LIST(NIN,IB)%POLY(9)%NumPOINT         = 0

          BRICK_LIST(NIN,IB)%POLY(1)%FACE(1:6)%NumPOINT = 4
          BRICK_LIST(NIN,IB)%POLY(9)%FACE(1:6)%NumPOINT = 0

          BRICK_LIST(NIN,IB)%POLY(9)%FACE(1:6)%Adjacent_Cell(1) = 0
          BRICK_LIST(NIN,IB)%POLY(9)%FACE(1:6)%Adjacent_Cell(2) = 0
          BRICK_LIST(NIN,IB)%POLY(9)%FACE(1:6)%Adjacent_Cell(3) = 0
          BRICK_LIST(NIN,IB)%POLY(9)%FACE(1:6)%Adjacent_Cell(4) = 0
          BRICK_LIST(NIN,IB)%POLY(9)%FACE(1:6)%Adjacent_Cell(5) = 0

          BRICK_LIST(NIN,IB)%POLY(9)%FACE(1:6)%NAdjCell = 0
          BRICK_LIST(NIN,IB)%POLY(1)%ListNodID(1:8)   = (/1,2,3,4,5,6,7,8/)
          BRICK_LIST(NIN,IB)%POLY(9)%ListNodID(1:8)   = 0
          BRICK_LIST(NIN,IB)%POLY(9)%Vnew             = ZERO

          BRICK_LIST(NIN,IB)%POLY(1)%FACE(1:6)%Surf   = BRICK_LIST(NIN,IB)%Face_Brick(1:6)
          !BRICK_LIST(NIN,IB)%POLY(1)%FACE(  J)%Surf   = ZERO
          BRICK_LIST(NIN,IB)%POLY(9)%FACE(1:6)%Surf   = ZERO

          BRICK_LIST(NIN,IB)%PCUT(1)%N(1:3)           = ZERO
          BRICK_LIST(NIN,IB)%PCUT(1)%B(1:3)           = ZERO
          BRICK_LIST(NIN,IB)%PCUT(1)%SCUT             = ZERO
          BRICK_LIST(NIN,IB)%PCUT(1)%SEFF             = ZERO
          BRICK_LIST(NIN,IB)%PCUT(1)%P(1:3,1:6)       = ZERO
          BRICK_LIST(NIN,IB)%PCUT(1)%NP               = 0
          BRICK_LIST(NIN,IB)%SECTYPE(1)               = 'REMOVED 0-CELL'

          BRICK_LIST(NIN,IB)%POLY(1)%WhereIsMain(1:2) = 0
          BRICK_LIST(NIN,IB)%POLY(1)%WhereIsMain(3)   = ID
          BRICK_LIST(NIN,IB)%POLY(1)%WhereIsMain(4)   = IB

          !BRICK_LIST(NIN,IB)%POLY(1)%CellCENTER(1:3)  = BRICK_LIST(NIN,IB)%POLY(1)%CellCENTER(1:3)   !keep same one

  !optim : no time to optim, will be treated later, meanwhile a second pass is done in sinit22.F
          !---UPDATING ADJACENT CELL
c          NAdjv = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(Jv)%NAdjCell
c          DO IADJv=1,NADJv
c            IF (BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(Jv)%Adjacent_Cell(IADJv)==ICELL_TARGET)EXIT
c          ENDDO
c          BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(Jv)%Adjacent_Cell(IADJv) = 1     !it remains in this case only the full brick.
c          !---local brick
c          !if the new merged cell (after destroy) has already IBv.ICELLv as an adjacent cell, do not add it.
c          NADj = BRICK_LIST(NIN,IB)%POLY(1)%FACE(J)%NAdjCell
c          bFOUND = .FALSE.
c          DO IADJ=1,NADJ
c            IF (BRICK_LIST(NIN,IBv)%POLY(1)%FACE(J)%Adjacent_Cell(IADJ)==ICELLv) bFOUND = .TRUE.
c          ENDDO
c          IF(.NOT.bFOUND)THEN
c            NADj = NADj + 1
c            BRICK_LIST(NIN,IB)%POLY(1)%FACE(J)%NAdjCell           = NADj
c            BRICK_LIST(NIN,IB)%POLY(1)%FACE(J)%Adjacent_Cell(IADJ) = ICELLv
c          ENDIF

        ELSEIF(NBCUT==1 .AND. ICELL_TARGET==1)THEN
          !--------------------------------------------------------------------------------!
          ! CASE OF TWO CELLS {1,9}  : MOVE CELL 9 TO 1 AND DESTROY CELL 9                 !
          !--------------------------------------------------------------------------------!

          !---SWITCH AND ERASE DATA FROM CELL 9

          BRICK_LIST(NIN,IB)%NBCUT                    = 0

          BRICK_LIST(NIN,IB)%SECID_Cell(1)            = 0

          BRICK_LIST(NIN,IB)%NODE(1:8)%WhichCell      = 1

          BRICK_LIST(NIN,IB)%POLY(1)%NumNOD           = 8
          BRICK_LIST(NIN,IB)%POLY(9)%NumNOD           = 0

          BRICK_LIST(NIN,IB)%POLY(1)%NumPOINT         = 8
          BRICK_LIST(NIN,IB)%POLY(9)%NumPOINT         = 0

          BRICK_LIST(NIN,IB)%POLY(1)%FACE(1:6)%NumPOINT  = 4
          BRICK_LIST(NIN,IB)%POLY(9)%FACE(1:6)%NumPOINT  = 0

          BRICK_LIST(NIN,IB)%POLY(1)%WhereIsMain(1:2) = 0
          BRICK_LIST(NIN,IB)%POLY(1)%WhereIsMain(3)   = ID
          BRICK_LIST(NIN,IB)%POLY(1)%WhereIsMain(4)   = IB

          DO K=1,6
          BRICK_LIST(NIN,IB)%POLY(1)%FACE(K)%Adjacent_Cell(1) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(K)%Adjacent_Cell(1)
          BRICK_LIST(NIN,IB)%POLY(1)%FACE(K)%Adjacent_Cell(2) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(K)%Adjacent_Cell(2)
          BRICK_LIST(NIN,IB)%POLY(1)%FACE(K)%Adjacent_Cell(3) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(K)%Adjacent_Cell(3)
          BRICK_LIST(NIN,IB)%POLY(1)%FACE(K)%Adjacent_Cell(4) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(K)%Adjacent_Cell(4)
          BRICK_LIST(NIN,IB)%POLY(1)%FACE(K)%Adjacent_Cell(5) = BRICK_LIST(NIN,IB)%POLY(9)%FACE(K)%Adjacent_Cell(5)
          ENDDO !next K

          DO K=1,6
          BRICK_LIST(NIN,IB)%POLY(9)%FACE(K)%Adjacent_Cell(1) = 0
          BRICK_LIST(NIN,IB)%POLY(9)%FACE(K)%Adjacent_Cell(2) = 0
          BRICK_LIST(NIN,IB)%POLY(9)%FACE(K)%Adjacent_Cell(3) = 0
          BRICK_LIST(NIN,IB)%POLY(9)%FACE(K)%Adjacent_Cell(4) = 0
          BRICK_LIST(NIN,IB)%POLY(9)%FACE(K)%Adjacent_Cell(5) = 0
          ENDDO !next K

          BRICK_LIST(NIN,IB)%POLY(1)%FACE(1:6)%NAdjCell = BRICK_LIST(NIN,IB)%POLY(9)%FACE(1:6)%NAdjCell
          BRICK_LIST(NIN,IB)%POLY(9)%FACE(1:6)%NAdjCell = 0

          BRICK_LIST(NIN,IB)%POLY(1)%ListNodID(1:8)   = (/1,2,3,4,5,6,7,8/)
          BRICK_LIST(NIN,IB)%POLY(9)%ListNodID(1:8)   = 0

          BRICK_LIST(NIN,IB)%POLY(1)%Vnew             = BRICK_LIST(NIN,IB)%POLY(9)%Vnew
          BRICK_LIST(NIN,IB)%POLY(9)%Vnew             = ZERO

          BRICK_LIST(NIN,IB)%POLY(1)%FACE(1:6)%Surf   = BRICK_LIST(NIN,IB)%Face_Brick(1:6)
          !BRICK_LIST(NIN,IB)%POLY(1)%FACE(  J)%Surf   = ZERO
          BRICK_LIST(NIN,IB)%POLY(9)%FACE(1:6)%Surf   = ZERO

          BRICK_LIST(NIN,IB)%PCUT(1)%N(1:3)           = ZERO
          BRICK_LIST(NIN,IB)%PCUT(1)%B(1:3)           = ZERO
          BRICK_LIST(NIN,IB)%PCUT(1)%SCUT             = ZERO
          BRICK_LIST(NIN,IB)%PCUT(1)%SEFF             = ZERO
          BRICK_LIST(NIN,IB)%PCUT(1)%P(1:3,1:6)       = ZERO
          BRICK_LIST(NIN,IB)%PCUT(1)%NP               = 0

          BRICK_LIST(NIN,IB)%POLY(1)%CellCENTER(1:3)  = BRICK_LIST(NIN,IB)%POLY(9)%CellCENTER(1:3)

          BRICK_LIST(NIN,IB)%SECTYPE(1)               = 'REMOVED 0-CELL'

  !optim : no time to optim, will be treated later, meanwhile a second pass is done in sinit22.F
c           !---UPDATING ADJACENT CELL
c          !adjacent brick
c          NAdjv = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(Jv)%NAdjCell
c          DO IADJv=1,NADJv
c            IF (BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(Jv)%Adjacent_Cell(IADJv)==ICELL_TARGET)EXIT
c          ENDDO
c          BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(Jv)%Adjacent_Cell(IADJv) = 1 !it remains in this case only the full brick.
c          !---local brick
c          !if the new merged cell (after destroy) has already IBv.ICELLv as an adjacent cell, do not add it.
c          NADj = BRICK_LIST(NIN,IB)%POLY(1)%FACE(J)%NAdjCell
c          bFOUND = .FALSE.
c          DO IADJ=1,NADJ
c            IF (BRICK_LIST(NIN,IBv)%POLY(1)%FACE(J)%Adjacent_Cell(IADJ)/=ICELL_TARGET) EXIT
c          ENDDO
c          IF(IADJ<=NADJ)THEN
c          !traiter ici l'adjacence en 3D : boucler sur toutes les voisines de toutes les faces, si = 1ou 9 selon ICELL_TARGET, mettre   jour
c          a
c            BRICK_LIST(NIN,IB)%POLY(1)%FACE(J)%Adjacent_Cell(IADJ) = ICELLv
c          ENDIF
        ELSE
          !--------------------------------------------------------------------------------!
          ! CASE OF MORE THAN TWO CELLS {1,2,..,9}                                         !
          !--------------------------------------------------------------------------------!
          !ICELL_TARGET CAN'T BE CELL 9 : ENSURED BY PARTIIONNING CONSTRUCTION
          !FOUR STEPS :
          !  -1- KEEP CELLS FROM 1:ICELL_TARGET-1
          !  -2- SHIFT CELLS FROM (ICELL_TARGET+1 : NBCUT) TO ICELL_TARGET:(NBCUT-1)
          !  -3- ERASE CELL ICELL_TARGET (FOR DEBUG PURPOSE : RESET IT AND TAG AS REMOVED ONE)
          !  -4- UPDATE CELL 9 ACCORDINGLY
          !  -5- UPDATE ADJACENCY
          FACE_CELL(1:6)     = BRICK_LIST(NIN,IB)%POLY(ICELL_TARGET)%FACE(1:6)%Surf
          NumNODES_Cell      = BRICK_LIST(NIN,IB)%POLY(ICELL_TARGET)%NumNOD
          NumIntP            = BRICK_LIST(NIN,IB)%POLY(ICELL_TARGET)%NumPOINT - NumNODES_Cell
          NumPOINT_Face(1:6) = BRICK_LIST(NIN,IB)%POLY(ICELL_TARGET)%FACE(1:6)%NumPOINT
          ListNodID(1:8)     = BRICK_LIST(NIN,IB)%POLY(ICELL_TARGET)%ListNodID(1:8)
          !---STEP 1
          ICELL = ICELL_TARGET - 1
          !---STEP 2
          DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} \ {9}
            ICELL = ICELL +1
            IF (ICELL>=NCELL)EXIT
            BRICK_LIST(NIN,IB)%SECID_Cell(ICELL)                        = BRICK_LIST(NIN,IB)%SECID_Cell(ICELL+1)
            BRICK_LIST(NIN,IB)%POLY(ICELL)%NumNOD                       = BRICK_LIST(NIN,IB)%POLY(ICELL+1)%NumNOD
            BRICK_LIST(NIN,IB)%POLY(ICELL)%NumPOINT                     = BRICK_LIST(NIN,IB)%POLY(ICELL+1)%NumPOINT
            BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%NumPOINT           = BRICK_LIST(NIN,IB)%POLY(ICELL+1)%FACE(1:6)%NumPOINT
            DO K=1,6
            BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%Adjacent_Cell(1) = BRICK_LIST(NIN,IB)%POLY(ICELL+1)%FACE(K)%Adjacent_Cell(1)
            BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%Adjacent_Cell(2) = BRICK_LIST(NIN,IB)%POLY(ICELL+1)%FACE(K)%Adjacent_Cell(2)
            BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%Adjacent_Cell(3) = BRICK_LIST(NIN,IB)%POLY(ICELL+1)%FACE(K)%Adjacent_Cell(3)
            BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%Adjacent_Cell(4) = BRICK_LIST(NIN,IB)%POLY(ICELL+1)%FACE(K)%Adjacent_Cell(4)
            BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%Adjacent_Cell(5) = BRICK_LIST(NIN,IB)%POLY(ICELL+1)%FACE(K)%Adjacent_Cell(5)
            ENDDO
            BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1)%NAdjCell             = BRICK_LIST(NIN,IB)%POLY(ICELL+1)%FACE(1)%NAdjCell
            BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:8)               = BRICK_LIST(NIN,IB)%POLY(ICELL+1)%ListNodID(1:8)
            BRICK_LIST(NIN,IB)%POLY(ICELL)%Vnew                         = BRICK_LIST(NIN,IB)%POLY(ICELL+1)%Vnew
            BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Surf               = BRICK_LIST(NIN,IB)%POLY(ICELL+1)%FACE(1:6)%Surf
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%N(1:3)                       = BRICK_LIST(NIN,IB)%PCUT(ICELL+1)%N(1:3)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%B(1:3)                       = BRICK_LIST(NIN,IB)%PCUT(ICELL+1)%B(1:3)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%SCUT                         = BRICK_LIST(NIN,IB)%PCUT(ICELL+1)%SCUT
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%SEFF                         = BRICK_LIST(NIN,IB)%PCUT(ICELL+1)%SEFF
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,1:6)                   = BRICK_LIST(NIN,IB)%PCUT(ICELL+1)%P(1:3,1:6)
            BRICK_LIST(NIN,IB)%PCUT(ICELL)%NP                           = BRICK_LIST(NIN,IB)%PCUT(ICELL+1)%NP
            BRICK_LIST(NIN,IB)%SECTYPE(ICELL)                           = BRICK_LIST(NIN,IB)%SECTYPE(ICELL+1)
            BRICK_LIST(NIN,IB)%POLY(ICELL)%CellCENTER(1:3)              = BRICK_LIST(NIN,IB)%POLY(ICELL+1)%CellCENTER(1:3)
          ENDDO!next ICELL
          !---STEP 3
          ICELL = NBCUT
          BRICK_LIST(NIN,IB)%SECID_Cell(ICELL)            = 0
          BRICK_LIST(NIN,IB)%POLY(ICELL)%NumNOD           = 0
          BRICK_LIST(NIN,IB)%POLY(ICELL)%NumPOINT         = 0
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%NumPOINT = 0
          DO K=1,6
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%Adjacent_Cell(1) = 0
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%Adjacent_Cell(2) = 0
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%Adjacent_Cell(3) = 0
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%Adjacent_Cell(4) = 0
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(K)%Adjacent_Cell(5) = 0
          ENDDO
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%NAdjCell = 0
          BRICK_LIST(NIN,IB)%POLY(ICELL)%ListNodID(1:8)   = 0
          BRICK_LIST(NIN,IB)%POLY(ICELL)%Vnew             = ZERO
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE(1:6)%Surf   = ZERO
          BRICK_LIST(NIN,IB)%POLY(ICELL)%FACE0%Surf       = ZERO
          BRICK_LIST(NIN,IB)%PCUT(ICELL)%N(1:3)           = ZERO
          BRICK_LIST(NIN,IB)%PCUT(ICELL)%B(1:3)           = ZERO
          BRICK_LIST(NIN,IB)%PCUT(ICELL)%SCUT             = ZERO
          BRICK_LIST(NIN,IB)%PCUT(ICELL)%SEFF             = ZERO
          BRICK_LIST(NIN,IB)%PCUT(ICELL)%P(1:3,1:6)       = ZERO
          BRICK_LIST(NIN,IB)%PCUT(ICELL)%NP               = 0
          BRICK_LIST(NIN,IB)%SECTYPE(ICELL)               = 'REMOVED 0-CELL'
          !---STEP 4
          BRICK_LIST(NIN,IB)%NBCUT = NBCUT - 1
          DO INOD=1,8
            IF(BRICK_LIST(NIN,IB)%NODE(INOD)%WhichCell==ICELL_TARGET)BRICK_LIST(NIN,IB)%NODE(INOD)%WhichCell=9
          ENDDO
          DO I=1,6
            !merge faces of destroyed cell with cell 9
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(I)%Surf     = BRICK_LIST(NIN,IB)%POLY(9)%FACE(I)%Surf + Face_Cell(I)
          ENDDO
          NListNod9                                     = BRICK_LIST(NIN,IB)%POLY(9)%NumNOD
          BRICK_LIST(NIN,IB)%POLY(9)%NumNOD             = BRICK_LIST(NIN,IB)%POLY(9)%NumNOD + NumNODES_Cell
          BRICK_LIST(NIN,IB)%POLY(9)%NumPOINT           = BRICK_LIST(NIN,IB)%POLY(9)%NumPOINT - NumIntP
          DO I=1,6
            BRICK_LIST(NIN,IB)%POLY(9)%FACE(I)%NumPOINT = BRICK_LIST(NIN,IB)%POLY(9)%FACE(I)%NumPOINT +  NumPOINT_Face(I)
          ENDDO
          BRICK_LIST(NIN,IB)%POLY(9)%ListNodID(NListNod9+1:8) = ListNodID(1:NumNODES_Cell)


          BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(1:4) = BRICK_LIST(NIN,IB)%POLY(9)%WhereIsMain(1:4)


          !---STEP 5
  !optim : no time to optim, will be treated later, meanwhile a second pass is done in sinit22.F
c            !---adjacent brick
c          NAdjv = BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(Jv)%NAdjCell
c          DO IADJv=1,NADJv
c            IF (BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(Jv)%Adjacent_Cell(IADJv)==ICELL_TARGET)EXIT
c          ENDDO
c          BRICK_LIST(NIN,IBv)%POLY(ICELLv)%FACE(Jv)%Adjacent_Cell(IADJv) = 9
c             !---local brick
c          !if the new merged cell (after destroy) has already IBv.ICELLv as an adjacent cell, do not add it.
c          NADj = BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%NAdjCell
c          bFOUND = .FALSE.
c          DO IADJ=1,NADJ
c            IF (BRICK_LIST(NIN,IBv)%POLY(9)%FACE(J)%Adjacent_Cell(IADJ)==ICELLv) bFOUND = .TRUE.
c          ENDDO
c          IF(.NOT.bFOUND)THEN
c            NADj = NADj + 1
c            BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%NAdjCell           = NADj
c            BRICK_LIST(NIN,IB)%POLY(9)%FACE(J)%Adjacent_Cell(IADJ) = ICELLv
c          ENDIF
        ENDIF



      END SUBROUTINE
