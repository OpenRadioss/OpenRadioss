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
Chd|  W_BUFBRIC_22                  source/interfaces/inter3d1/w_bufbric_22.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE W_BUFBRIC_22()
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
#include      "units_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real          :: BID
      INTEGER          :: NIN, I, J, K, IERR1, ISIZ
      INTEGER          :: Isectype(14)  
      CHARACTER*14     :: string
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      NB  = 0
      
      IF(.NOT.ALLOCATED(LIST_B_OLD))THEN   
        ALLOCATE(LIST_B_OLD(INT22,I22LEN),STAT=IERR1)
        IF (IERR1.NE.0) GOTO 1000        
        LIST_B_OLD(:,:) = 0
      ENDIF   
      
      IF(.NOT.ALLOCATED(NBOLD))THEN   
        ALLOCATE(NBOLD(INT22)            ,STAT=IERR1)
        IF (IERR1.NE.0) GOTO 1000       
        NBOLD(:) = 0
      ENDIF        
      
      IF(.NOT.ALLOCATED(BRICK_LIST))THEN
        ALLOCATE(BRICK_LIST(INT22,I22LEN),STAT=IERR1)
        IF (IERR1.NE.0) GOTO 1000 
        
        BRICK_LIST(:,:)%ID                                = -1
        BRICK_LIST(:,:)%NG                                = -2
        BRICK_LIST(:,:)%IDLOC                             = -3                
        BRICK_LIST(:,:)%ICODE                             = -4
        BRICK_LIST(:,:)%OLD_ICODE                         = -5        
        BRICK_LIST(:,:)%IDBLE                             = -6
        BRICK_LIST(:,:)%NBITS                             = -7
        BRICK_LIST(:,:)%NPQTS                             = -8
        BRICK_LIST(:,:)%NBCUT                             = -9
        BRICK_LIST(:,:)%MainID                          = -10        
        BRICK_LIST(:,:)%WasCut                            = -11
        BRICK_LIST(:,:)%NewInBuffer                       = 0
        BRICK_LIST(:,:)%OldMainStrongNode               = -13
        BRICK_LIST(:,:)%MLW                               = -14
        BRICK_LIST(:,:)%ITASK                             = -15
        
        DO J=1,8 ; BRICK_LIST(:,:)%SecId_Cell(J)          = -16      ;ENDDO                                         
        DO J=1,8 ; BRICK_LIST(:,:)%NODE(J)%OLD_WhichCell  = -17      ;ENDDO    
        DO J=1,8 ; BRICK_LIST(:,:)%NODE(J)%WhichCell      = -18      ;ENDDO    
        DO J=1,9 ; BRICK_LIST(:,:)%POLY(J)%NumNOD         = -19      ;ENDDO    
        DO J=1,9 ; BRICK_LIST(:,:)%POLY(J)%NumPOINT       = -20      ;ENDDO  
        
        DO I=1,6
        DO J=1,9 ; BRICK_LIST(:,:)%POLY(J)%FACE(I)%NumPOINT = -21       ;ENDDO          
        ENDDO;
        
        BRICK_LIST(:,:)%Seg_add_LFT                       = -22    
        BRICK_LIST(:,:)%Seg_add_LLT                       = -23   
            
        DO I=1,5
        DO J=1,6 ; BRICK_LIST(:,:)%Adjacent_Brick(J,I)    = -24      ;ENDDO  
        ENDDO
         
        DO I=1,6 
        DO J=1,9 
        DO K=1,5 ; BRICK_LIST(:,:)%POLY(J)%FACE(I)%Adjacent_Cell(K)   = -25      ;ENDDO;
        ENDDO 
        ENDDO
        
        DO I=1,9 
        DO J=1,6 ; BRICK_LIST(:,:)%POLY(I)%FACE(J)%NAdjCell  = -26      ;ENDDO;
        ENDDO
                
        DO I=1,8
        DO J=1,9 ; BRICK_LIST(:,:)%POLY(J)%ListNodID(I)   = -27      ;ENDDO        
        ENDDO
        
        DO J=1,9 ; BRICK_LIST(:,:)%POLY(J)%IsMain       = -28      ;ENDDO             

        DO J=1,8 ; BRICK_LIST(:,:)%NODE(J)%NodWasMain   = -29      ;ENDDO         

        DO I=1,4
        DO J=1,9 ; BRICK_LIST(:,:)%POLY(J)%WhereIsMain(I) = -30    ;ENDDO              
        ENDDO

        DO I=1,8 ; BRICK_LIST(:,:)%NODE(I)%WhereWasMain   = -31      ;ENDDO              
        
        DO I=1,3
        DO J=1,5 ; BRICK_LIST(:,:)%MergeTarget(I,J)       = -32      ;ENDDO                      
        ENDDO 
        
        BRICK_LIST(:,:)%NTarget                           = -33

        DO I=1,6 ; BRICK_LIST(:,:)%ClosedSurf(I)          = -34      ;ENDDO  

        DO I=1,9 ; BRICK_LIST(:,:)%POLY(I)%ID_FREE_NODE   = -35      ;ENDDO 
       
        DO I=1,6
        DO J=1,9
        DO K=1,5 ; BRICK_LIST(:,:)%POLY(J)%FACE(I)%Adjacent_FLUX(K)   = -102.D00 ;ENDDO                 
        ENDDO
        ENDDO

        DO I=1,6
        DO J=1,9
        DO K=1,5 ; BRICK_LIST(:,:)%POLY(J)%FACE(I)%Adjacent_upwFLUX(K)= -103.D00 ;ENDDO                 
        ENDDO
        ENDDO

        DO I=1,9 ; BRICK_LIST(:,:)%POLY(I)%Adjacent_FLU1  = -104.D00 ;ENDDO                 

        DO I=1,6
        DO J=1,3 ; BRICK_LIST(:,:)%N(I,J)                 = -105.D00 ;ENDDO                 
        ENDDO

        DO J=1,9 ; BRICK_LIST(:,:)%POLY(J)%DDVOL          = -106.D00 ;ENDDO
        
        DO J=1,9 ; BRICK_LIST(:,:)%POLY(J)%PHI            = -107.D00 ;ENDDO
        DO J=1,9 ; BRICK_LIST(:,:)%POLY(J)%DPHI           = -108.D00 ;ENDDO
        
        DO I=1,9
        DO J=1,4 ; BRICK_LIST(:,:)%POLY(I)%VFRACm(J)      = -109.D00 ;ENDDO        
        ENDDO
                
        BRICK_LIST(:,:)%Vnew_SCell                        = -110.D00                
        BRICK_LIST(:,:)%Vold_SCell                        = -111.D00        
        DO J=1,9 ; BRICK_LIST(:,:)%POLY(J)%Vnew           = -112.D00 ;ENDDO
        DO J=1,9 ; BRICK_LIST(:,:)%POLY(J)%Vold           = -113.D00 ;ENDDO
        DO J=1,9 ; BRICK_LIST(:,:)%POLY(J)%OLD_Vnew       = -114.D00 ;ENDDO

        DO I=1,3          
         BRICK_LIST(:,:)%SCellCenter(I)                   = -114.5D00
        ENDDO

        DO I=1,3         
        DO J=1,9 ; BRICK_LIST(:,:)%POLY(J)%CellCenter(I)  = -115.D00 ;ENDDO                  
        ENDDO

        DO K=1,9
        DO I=1,3          
        DO J=1,6 ; BRICK_LIST(:,:)%POLY(K)%FACE(J)%Center(I) = -115.5D00;ENDDO                  
        ENDDO
        ENDDO
           
        DO J=1,6 ; 
        DO I=1,9 ; BRICK_LIST(:,:)%POLY(I)%FACE(J)%Surf   = -116.D00 ;ENDDO;
        ENDDO 

        DO K=1,9
        DO J=1,3
        DO I=1,6 ; BRICK_LIST(:,:)%POLY(K)%FACE(I)%Vel(J) = -117.D00 ;ENDDO;
        ENDDO
        ENDDO

        DO J=1,6 ; BRICK_LIST(:,:)%Face_Brick(J)          = -118.D00 ;ENDDO 

        BRICK_LIST(:,:)%UncutVol                          = -119.D00

        DO K=1,9
        DO I=1,6                        
        DO J=1,3 ; BRICK_LIST(:,:)%POLY(K)%FACE(I)%F_FACE(J)  = -121.D00 ;ENDDO                  
        ENDDO
        ENDDO        

        DO J=1,3 ; BRICK_LIST(:,:)%FCELL(J)               = -122.D00 ;ENDDO                  

        DO J=1,3 ; BRICK_LIST(:,:)%FEXT_CELL(J)           = -123.D00 ;ENDDO                  
        
        DO J=0,6 ; BRICK_LIST(:,:)%SIG(J)                 = -124.D00 ;ENDDO          

        DO I=0,2
        DO J=1,9 ; BRICK_LIST(:,:)%POLY(J)%DVOL(I)        = -125.D00 ;ENDDO                  
        ENDDO
        BRICK_LIST(:,:)%DVOL                              = -125.D00
        
        
        DO J=1,2
        DO I=1,6 ; BRICK_LIST(:,:)%Poly9woNodes(I,J)      = -126.D00 ;ENDDO                  
        ENDDO

        BRICK_LIST(:,:)%ADJ_ELEMS%Num                     = -30                  
        DO J=1,48
          BRICK_LIST(:,:)%ADJ_ELEMS%Num_inv(J)            = -31
          BRICK_LIST(:,:)%ADJ_ELEMS%IV(J)                 = -32
          BRICK_LIST(:,:)%ADJ_ELEMS%IB(J)                 = -33
          BRICK_LIST(:,:)%ADJ_ELEMS%ICELL(J)              = -34 
          BRICK_LIST(:,:)%ADJ_ELEMS%SecndFACE(J)          = -35    
        ENDDO !next J         
         
        DO I=1,14
        BRICK_LIST(:,:)%PCUT(I)%NP                        = -130.D00                             
        BRICK_LIST(:,:)%PCUT(I)%N(1)                      = -131.D00 
        BRICK_LIST(:,:)%PCUT(I)%N(2)                      = -132.D00  
        BRICK_LIST(:,:)%PCUT(I)%N(3)                      = -133.D00  
        BRICK_LIST(:,:)%PCUT(I)%B(1)                      = -134.D00  
        BRICK_LIST(:,:)%PCUT(I)%B(2)                      = -135.D00  
        BRICK_LIST(:,:)%PCUT(I)%B(3)                      = -136.D00             
        BRICK_LIST(:,:)%PCUT(I)%SCUT(1)                   = -137.D00
        BRICK_LIST(:,:)%PCUT(I)%SEFF(1)                   = -138.D00
        DO J=1,8
        BRICK_LIST(:,:)%PCUT(I)%P(1,J)                    = -139.D00
        BRICK_LIST(:,:)%PCUT(I)%P(2,J)                    = -140.D00 
        BRICK_LIST(:,:)%PCUT(I)%P(3,J)                    = -141.D00 
        ENDDO !next J
        ENDDO !next I  
        
        BRICK_LIST(:,:)%SecndList%VOL_Unmerged            = -142.D00
        BRICK_LIST(:,:)%SecndList%Num                     = -40                  
        BRICK_LIST(:,:)%SecndList%NumSecndNodes           = -41                          
        DO J=1,24
          BRICK_LIST(:,:)%SecndList%FM(J)                 = -42
          BRICK_LIST(:,:)%SecndList%FV(J)                 = -43          
          BRICK_LIST(:,:)%SecndList%IV(J)                 = -44
          BRICK_LIST(:,:)%SecndList%IBV(J)                = -45 
          BRICK_LIST(:,:)%SecndList%ICELLv(J)             = -46                     
          BRICK_LIST(:,:)%SecndList%VOL(J)                = -1.0D00     
          BRICK_LIST(:,:)%SecndList%NumNOD_Cell(J)        = -47 
          do k=1,8
          BRICK_LIST(:,:)%SecndList%ListNodID(J,k)        = -48               
          enddo
        ENDDO !next J                  

        DO J=1,12
          BRICK_LIST(:,:)%Edge(J)%NODE(1)                 = -49
          BRICK_LIST(:,:)%Edge(J)%NODE(2)                 = -50          
          BRICK_LIST(:,:)%Edge(J)%NBCUT                   = 0          
          BRICK_LIST(:,:)%Edge(J)%CUTSHELL(1)             = -59
          BRICK_LIST(:,:)%Edge(J)%CUTSHELL(2)             = -59         
          BRICK_LIST(:,:)%Edge(J)%CUTCOOR(1)              = -60.0D00
          BRICK_LIST(:,:)%Edge(J)%CUTCOOR(2)              = -60.0D00
          BRICK_LIST(:,:)%Edge(J)%CUTPOINT(1,1)           = -61.0D00
          BRICK_LIST(:,:)%Edge(J)%CUTPOINT(2,1)           = -61.0D00
          BRICK_LIST(:,:)%Edge(J)%CUTPOINT(3,1)           = -61.0D00
          BRICK_LIST(:,:)%Edge(J)%CUTPOINT(1,2)           = -61.0D00
          BRICK_LIST(:,:)%Edge(J)%CUTPOINT(2,2)           = -61.0D00
          BRICK_LIST(:,:)%Edge(J)%CUTPOINT(3,2)           = -61.0D00                                             
          BRICK_LIST(:,:)%Edge(J)%CUTVEL(1,1)             = -62.0D00
          BRICK_LIST(:,:)%Edge(J)%CUTVEL(2,1)             = -62.0D00
          BRICK_LIST(:,:)%Edge(J)%CUTVEL(3,1)             = -62.0D00
          BRICK_LIST(:,:)%Edge(J)%CUTVEL(1,2)             = -62.0D00
          BRICK_LIST(:,:)%Edge(J)%CUTVEL(2,2)             = -62.0D00
          BRICK_LIST(:,:)%Edge(J)%CUTVEL(3,2)             = -62.0D00
          BRICK_LIST(:,:)%Edge(J)%VECTOR(1)               = -63.0D00
          BRICK_LIST(:,:)%Edge(J)%VECTOR(2)               = -63.0D00
          BRICK_LIST(:,:)%Edge(J)%VECTOR(3)               = -63.0D00
          BRICK_LIST(:,:)%Edge(J)%LEN                     = -64.0D00          
        ENDDO
         
        BRICK_LIST(:,:)%SECTYPE(1)                        = 'init-1        '           
        BRICK_LIST(:,:)%SECTYPE(2)                        = 'init-2        '        
        BRICK_LIST(:,:)%SECTYPE(3)                        = 'init-3        '  
        BRICK_LIST(:,:)%SECTYPE(4)                        = 'init-4        '  
        BRICK_LIST(:,:)%SECTYPE(5)                        = 'init-5        '  
        BRICK_LIST(:,:)%SECTYPE(6)                        = 'init-6        '  
        BRICK_LIST(:,:)%SECTYPE(7)                        = 'init-7        '  
        BRICK_LIST(:,:)%SECTYPE(8)                        = 'init-8        '  
      ENDIF !(.NOT.ALLOCATED(BRICK_LIST))

        BRICK_LIST(:,:)%bakMAT%rho       = -65D00
        BRICK_LIST(:,:)%bakMAT%rhoE      = -66D00     
        BRICK_LIST(:,:)%bakMAT%rhoU(1)   = -67D00
        BRICK_LIST(:,:)%bakMAT%rhoU(2)   = -67D00
        BRICK_LIST(:,:)%bakMAT%rhoU(3)   = -67D00
        BRICK_LIST(:,:)%bakMAT%sig(1)    = -68D00 
        BRICK_LIST(:,:)%bakMAT%sig(2)    = -68D00
        BRICK_LIST(:,:)%bakMAT%sig(3)    = -68D00
        BRICK_LIST(:,:)%bakMAT%sig(4)    = -68D00
        BRICK_LIST(:,:)%bakMAT%sig(5)    = -68D00
        BRICK_LIST(:,:)%bakMAT%sig(6)    = -68D00
        BRICK_LIST(:,:)%bakMAT%ssp       = -69D00
 
       ALLOCATE(OLD_SecndList(INT22,I22LEN) )
       DO NIN=1,INT22
       !=====%OLD_SecndList(NIN,1:I22LEN)
                   OLD_SecndList(NIN,1:I22LEN)%VOL_Unmerged    = ZERO                 
                   OLD_SecndList(NIN,1:I22LEN)%Num             = 0                
                   OLD_SecndList(NIN,1:I22LEN)%NumSecndNodes   = 0            
        DO J=1,24
                    OLD_SecndList(NIN,1:I22LEN)%FM(J)          = 0               
                    OLD_SecndList(NIN,1:I22LEN)%FV(J)          = 0               
                    OLD_SecndList(NIN,1:I22LEN)%IV(J)          = 0                  
                    OLD_SecndList(NIN,1:I22LEN)%IBV(J)         = 0                  
                    OLD_SecndList(NIN,1:I22LEN)%ICELLv(J)      = 0                  
                    OLD_SecndList(NIN,1:I22LEN)%VOL(J)         = ZERO                  
                    OLD_SecndList(NIN,1:I22LEN)%SURF_v(J)      = ZERO                  
                    OLD_SecndList(NIN,1:I22LEN)%NumNOD_Cell(J) = 0                  
                   DO K=1,8
                   OLD_SecndList(NIN,1:I22LEN)%ListNodID(J,K)  = 0                            
                   ENDDO
        ENDDO !next J
        ENDDO!next NIN

 
 
      
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
        BID  = ZERO
        IF(ISIZ.NE.0)THEN
          DO K=1,I22LEN
          DO I=1,ISIZ 
                   !CALL WRITE_DB ( BRICK_LIST(NIN,K)%bakMAT%UVAR(I)                      , I22LEN)
                   CALL WRITE_DB ( BID                      , 1)
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
        DEALLOCATE(OLD_SecndList)
  
        
      ENDDO !next NIN 

C----------------------------------------------- 
 1000 CONTINUE

        IF (IERR1.NE.0) THEN
          WRITE(IOUT,*)' ** ERROR IN MEMORY ALLOCATION (INTER22)'
          WRITE(ISTDO,*)' ** ERROR IN MEMORY ALLOCATION (INTER22)'
          CALL ARRET(2)
        END IF 
C-----------------------------------------------        
      END SUBROUTINE

