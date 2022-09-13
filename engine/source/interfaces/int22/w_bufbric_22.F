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
Chd|  W_BUFBRIC_22                  source/interfaces/int22/w_bufbric_22.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE W_BUFBRIC_22()
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandonned, and is not an official option.
C
C This subroutine is writing buffer in RESTART file.
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
#include      "com04_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER          :: NIN, I, J, K, ISIZ
      INTEGER          :: Isectype(14)  
      CHARACTER*14     :: string
!      EQUIVALENCE(Isectype,Csectype)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      !temporary arg copy is needed while calling READ_* subroutine (ifort & -check arg_temp_created)
      ! readability is better but this is only for restart file and also not a performance issue.

      DO NIN=1,INT22
       !=====general data
                   CALL WRITE_I_C( NBOLD(NIN)                                                   ,      1)                              
                   CALL WRITE_I_C( LIST_B_OLD(NIN,1:I22LEN)                                     , I22LEN)                              
       !=====cartesian cell data - INTEGER
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%ID                                  , I22LEN)                              
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%NG                                  , I22LEN)                              
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%IDLOC                               , I22LEN)                              
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%ICODE                               , I22LEN)                              
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%OLD_ICODE                           , I22LEN)                              
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%IDBLE                               , I22LEN)                              
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%NBITS                               , I22LEN)                              
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%NPQTS                               , I22LEN)                              
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%NBCUT                               , I22LEN)                              
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%MainID                            , I22LEN)                              
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%WasCut                              , I22LEN)                              
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%NewInBuffer                         , I22LEN)                              
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%OldMainStrongNode                 , I22LEN)                              
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%MLW                                 , I22LEN)                              
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%ITASK                               , I22LEN)                              
        DO J=1,8 ; CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%SecID_Cell(J)                       , I22LEN) ; ENDDO                      
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%Seg_add_LFT                         , I22LEN)                              
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%Seg_add_LLT                         , I22LEN)                              
        DO I=1,5
        DO J=1,6 ; CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%Adjacent_Brick(J,I)                 , I22LEN) ; ENDDO                      
        ENDDO
        DO I=1,3
        DO J=1,5 ; CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%MergeTarget(I,J)                   , I22LEN) ; ENDDO                     
        ENDDO
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%NTarget                             , I22LEN)                        
        DO I=1,6 ; CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%ClosedSurf(I)                      , I22LEN) ; ENDDO                      
                   CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%IsMErgeTarget                       , I22LEN)                        
       !=====cartesian cell data - REAL
        DO I=1,6
        DO J=1,3 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%N(I,J)                              , I22LEN) ; ENDDO                      
        ENDDO
                   CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%Vnew_SCell                          , I22LEN)                              
                   CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%Vold_SCell                          , I22LEN)                              
        DO I=1,3 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%SCellCenter(I)                      , I22LEN) ; ENDDO
        DO J=1,6 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%Face_Brick(J)                       , I22LEN) ; ENDDO
                   CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%UncutVol                            , I22LEN)
        DO J=1,3 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%FCELL(J)                            , I22LEN) ; ENDDO
        DO J=1,3 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%FEXT_CELL(J)                        , I22LEN) ; ENDDO        
        DO J=0,6 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%SIG(J)                              , I22LEN) ; ENDDO        
                   CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%RHOC                                , I22LEN)
                   CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%MACH                                , I22LEN)
                   CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%DVOL                                , I22LEN)
        DO J=1,2
        DO I=1,6 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%Poly9woNodes(I,J)                   , I22LEN) ; ENDDO        
        ENDDO
       !=====%POLY(1:9) - INTEGER
        DO J=1,9 ; CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%NumNOD                      , I22LEN) ; ENDDO                                 
        DO J=1,9 ; CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%NumPOINT                    , I22LEN) ; ENDDO                      
        DO I=1,8
        DO J=1,9 ; CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%ListNodID(I)               , I22LEN) ; ENDDO                  
        ENDDO        
        DO J=1,9 ; CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%IsMain                   , I22LEN) ; ENDDO                  
        DO I=1,4
        DO J=1,9 ; CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%WhereIsMain(I)           , I22LEN) ; ENDDO                  
        ENDDO
        DO J=1,9 ; CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%ID_FREE_NODE               , I22LEN) ; ENDDO                  
       !=====%POLY(1:9) - REAL
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%Adjacent_FLU1               , I22LEN) ; ENDDO           
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%DDVOL                       , I22LEN) ; ENDDO          
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%PHI                         , I22LEN) ; ENDDO  
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%dPHI                        , I22LEN) ; ENDDO                    
        DO J=1,9
        DO I=1,4 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%VFRACm(I)                   , I22LEN) ; ENDDO         
        ENDDO               
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%Vnew                        , I22LEN) ; ENDDO 
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%Vold                        , I22LEN) ; ENDDO 
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%OLD_Vnew                    , I22LEN) ; ENDDO 
        DO I=1,3
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%CellCenter(I)               , I22LEN) ; ENDDO           
        ENDDO
        DO I=0,2
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%DVOL(I)                     , I22LEN) ; ENDDO                
        ENDDO
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%cutFace                     , I22LEN) ; ENDDO                
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%DM                          , I22LEN) ; ENDDO                
       !=====%POLY(1:9)%FACE0
        DO J=1,9 ; CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE0%NumPOINT             , I22LEN) ; ENDDO    
        DO J=1,9 ; CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE0%NAdjCell             , I22LEN) ; ENDDO 
        DO I=1,3       
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE0%Center(I)             , I22LEN) ; ENDDO 
        ENDDO    
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE0%Surf                  , I22LEN) ; ENDDO 
        DO I=1,3        
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE0%F_FACE(I)             , I22LEN) ; ENDDO 
        ENDDO    
        DO I=1,9       
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE0%U_N(I)                , I22LEN) ; ENDDO 
        ENDDO 
       !=====%POLY(1:9)%FACE(1:6)
       DO K=1,6
        DO J=1,9 ; CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%NumPOINT           , I22LEN) ; ENDDO      
        DO J=1,9 ; CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%NAdjCell           , I22LEN) ; ENDDO      
        DO I=1,5       
        DO J=1,9 ; CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%Adjacent_Cell(I)   , I22LEN) ; ENDDO      
        ENDDO          
        DO I=1,5       
        DO J=1,9 ; CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%Adjacent_FLUX(I)   , I22LEN) ; ENDDO      
        ENDDO 
        DO I=1,5       
        DO J=1,9 ; CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%Adjacent_upwFLUX(I), I22LEN) ; ENDDO      
        ENDDO 
        DO I=1,3       
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%Center(I)           , I22LEN) ; ENDDO      
        ENDDO    
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%Surf                , I22LEN) ; ENDDO      
        DO I=1,3        
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%Vel(I)              , I22LEN) ; ENDDO      
        ENDDO    
        DO I=1,3        
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%F_FACE(I)           , I22LEN) ; ENDDO      
        ENDDO    
        DO J=1,9 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%POLY(J)%FACE(K)%U_N                 , I22LEN) ; ENDDO      
       ENDDO!next K
       !=====%NODE
        DO J=1,8 ; CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%NODE(J)%OLD_WhichCell               , I22LEN) ; ENDDO                 
        DO J=1,8 ; CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%NODE(J)%WhichCell                   , I22LEN) ; ENDDO                 
        DO J=1,8 ; CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%NODE(J)%NodWasMain                , I22LEN) ; ENDDO        
        DO I=1,8 ; CALL WRITE_I_C( BRICK_LIST(NIN,1:I22LEN)%NODE(I)%WhereWasMain              , I22LEN) ; ENDDO        
       !=====%ADJ_ELEMS
        CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%ADJ_ELEMS%Num                                 , I22LEN)                 
        DO J=1,48
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%ADJ_ELEMS%Num_inv(J)               , I22LEN)
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%ADJ_ELEMS%IV(J)                    , I22LEN)
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%ADJ_ELEMS%IB(J)                    , I22LEN)
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%ADJ_ELEMS%ICELL(J)                 , I22LEN)
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%ADJ_ELEMS%SecndFACE(J)             , I22LEN)          
        ENDDO !next J
       !=====%PCUT
        DO I=1,16
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%NP                         , I22LEN)                          
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%N(1)                       , I22LEN)
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%N(2)                       , I22LEN)
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%N(3)                       , I22LEN) 
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%B(1)                       , I22LEN)
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%B(2)                       , I22LEN)
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%B(3)                       , I22LEN)                
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%SCUT(1)                    , I22LEN)
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%SEFF(1)                    , I22LEN)      
                   DO J=1,8
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%P(1,J)                     , I22LEN)
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%P(2,J)                     , I22LEN)
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%P(3,J)                     , I22LEN)
                   ENDDO !next J
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%Vel(1)                     , I22LEN)
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%Vel(2)                     , I22LEN)
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%PCUT(I)%Vel(3)                     , I22LEN) 
        ENDDO !next J  
       !=====%SecndList
        CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%SecndList%VOL_Unmerged                        , I22LEN)
        CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%SecndList%Num                                 , I22LEN)                 
        CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%SecndList%NumSecndNodes                       , I22LEN)        
        DO J=1,24
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%SecndList%FM(J)                    , I22LEN)
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%SecndList%FV(J)                    , I22LEN)         
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%SecndList%IV(J)                    , I22LEN)
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%SecndList%IBV(J)                   , I22LEN)
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%SecndList%ICELLv(J)                , I22LEN)
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%SecndList%VOL(J)                   , I22LEN)  
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%SecndList%SURF_v(J)                , I22LEN)    
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%SecndList%NumNOD_Cell(J)           , I22LEN)
                   DO K=1,8
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%SecndList%ListNodID(J,K)           , I22LEN)                       
                   ENDDO
        ENDDO !next J
       !=====%Edge
        DO J=1,12
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%NODE(1)                    , I22LEN) 
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%NODE(2)                    , I22LEN)        
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%NBCUT                      , I22LEN)        
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTSHELL(1)                , I22LEN)  
                   CALL WRITE_I_C ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTSHELL(2)                , I22LEN)                 
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTCOOR(1)                 , I22LEN)
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTCOOR(2)                 , I22LEN)        
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTPOINT(1,1)              , I22LEN)        
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTPOINT(2,1)              , I22LEN)        
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTPOINT(3,1)              , I22LEN)                         
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTPOINT(1,2)              , I22LEN)        
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTPOINT(2,2)              , I22LEN)        
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTPOINT(3,2)              , I22LEN)                         
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTVEL(1,1)                , I22LEN)        
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTVEL(2,1)                , I22LEN)        
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTVEL(3,1)                , I22LEN)                         
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTVEL(1,2)                , I22LEN)        
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTVEL(2,2)                , I22LEN)        
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%CUTVEL(3,2)                , I22LEN)
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%VECTOR(1)                  , I22LEN)        
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%VECTOR(2)                  , I22LEN)        
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%VECTOR(3)                  , I22LEN)                         
                   CALL WRITE_DB  ( BRICK_LIST(NIN,1:I22LEN)%Edge(J)%LEN                        , I22LEN)        
        ENDDO        
       !=====%SECTYPE
        DO J=1,8
          DO I=1,I22LEN
            string = BRICK_LIST(NIN,I)%SECTYPE(J)
            do k=1,14
            CALL WRITE_I_C( ICHAR(string(k:k)), 1)   
            enddo
          ENDDO
        ENDDO
       !=====%bakMAT
                   CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%bakMAT%rho                          , I22LEN) 
                   CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%bakMAT%rhoE                         , I22LEN)   
        DO I=1,3 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%bakMAT%rhoU(I)                      , I22LEN) ; ENDDO 
        DO I=1,6 ; CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%bakMAT%sig(I)                       , I22LEN) ; ENDDO 
                   CALL WRITE_DB ( BRICK_LIST(NIN,1:I22LEN)%bakMAT%ssp                          , I22LEN) 
        ISIZ = MAX (I22LAW37, I22LAW51)
        IF(ISIZ/=0)THEN
          DO K=1,I22LEN
          DO I=1,ISIZ  
                   CALL WRITE_DB ( BRICK_LIST(NIN,K)%bakMAT%UVAR(I)                             , 1)
          ENDDO 
          ENDDO
        ENDIF     

       !=====%OLD_SecndList(NIN,1:I22LEN)
                   CALL WRITE_DB  (OLD_SecndList(NIN,1:I22LEN)%VOL_Unmerged                     , I22LEN)   
                   CALL WRITE_I_C (OLD_SecndList(NIN,1:I22LEN)%Num                              , I22LEN)      
                   CALL WRITE_I_C (OLD_SecndList(NIN,1:I22LEN)%NumSecndNodes                    , I22LEN)   
        DO J=1,24
                   CALL WRITE_I_C ( OLD_SecndList(NIN,1:I22LEN)%FM(J)                           , I22LEN)      
                   CALL WRITE_I_C ( OLD_SecndList(NIN,1:I22LEN)%FV(J)                           , I22LEN)      
                   CALL WRITE_I_C ( OLD_SecndList(NIN,1:I22LEN)%IV(J)                           , I22LEN)      
                   CALL WRITE_I_C ( OLD_SecndList(NIN,1:I22LEN)%IBV(J)                          , I22LEN)      
                   CALL WRITE_I_C ( OLD_SecndList(NIN,1:I22LEN)%ICELLv(J)                       , I22LEN)      
                   CALL WRITE_DB  ( OLD_SecndList(NIN,1:I22LEN)%VOL(J)                          , I22LEN)      
                   CALL WRITE_DB  ( OLD_SecndList(NIN,1:I22LEN)%SURF_v(J)                       , I22LEN)      
                   CALL WRITE_I_C ( OLD_SecndList(NIN,1:I22LEN)%NumNOD_Cell(J)                  , I22LEN)      
                   DO K=1,8
                   CALL WRITE_I_C ( OLD_SecndList(NIN,1:I22LEN)%ListNodID(J,K)                  , I22LEN)                
                   ENDDO
        ENDDO !next J
  
        
      ENDDO !next NIN 
      
C-----------------------------------------------        
      END SUBROUTINE
