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
Chd|  NODALP                        source/output/anim/generate/nodalp.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        H3D_NODAL_SCALAR              source/output/h3d/h3d_results/h3d_nodal_scalar.F
Chd|-- calls ---------------
Chd|        ANINFLOW                      source/output/anim/generate/aninflow.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE ANIM_NODAL_P_ELEMS(IFUNC, WA4, IPARG,  ELBUF_TAB, IX , NIX  , NUMEL, ITAB , NV46, IS_WRITTEN_NODE)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     This suroutine computes nodal pressure for
C     ALE elements. In case of CEL coupling (inter22)
C     result is also calculated from cut cells.
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
C     Tested below during NG LOOP : IALEL > 0 
C        where IALEL =IPARG(7,NG)+IPARG(11,NG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD  
      USE I22BUFBRIC_MOD   
      USE I22EDGE_MOD    
      USE I22TRI_MOD           
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IFUNC, IPARG(NPARG,NGROUP),IX(NIX,NUMEL),ITAB(NUMNOD),NIX,NV46,NUMEL
      REAL,INTENT(INOUT) :: WA4(*)
      TYPE (ELBUF_STRUCT_),INTENT(IN), DIMENSION(NGROUP), TARGET :: ELBUF_TAB 
      INTEGER, INTENT(INOUT) :: IS_WRITTEN_NODE(NUMNOD)  
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IADI, IADR, I, ITYP, NINOUT, NNO, NEL, NELv,II1, II2,
     .        IR1, IR2, J, JJ, NNO_L, NNI_L, II3, II4, JJJ, NNI,
     .        IALEL,NNOD,IPOS,IV,NGv,IDLOCv,J1,J2,IBV
      INTEGER MLW, NG, KCVT, II(6), NBF, NBL, IB, ICELL, NIN, MCELL
      TYPE(G_BUFEL_)  ,POINTER :: GBUF,GBUFv     
      my_real, ALLOCATABLE, DIMENSION(:) :: SUM_WEIGHT    
      my_real P, WEIGHT
      INTEGER,DIMENSION(:,:), POINTER   :: pAdjBRICK      
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   This subroutine is setting nodal pressure requested by Engine keyword /ANIM/NODA/P
C   Several situation are possible. This one is dealing with nodal pressure from adjacent elements.
C     + LOOP OVER ELEM
C     |   GET ITS PRESSURE FROM ELEM BUFFER
C     |   USE ELEM TO SET MEAN PRESSURE ON ELEM NODES (WEIGHT = VOLUME)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      

         ALLOCATE(SUM_WEIGHT(NUMNOD))      
         SUM_WEIGHT = 0
         NNOD = NIX-3   !8-node brick or 4-node quad

         IF(INT22==0)THEN
         !---------------------------------------------------------!         
         !         EXPAND ELEM PRESSURE TO NODES                   !
         !---------------------------------------------------------!                                
           DO NG = 1, NGROUP
             NEL   =IPARG(2,NG)
             NFT   =IPARG(3,NG)
             ITYP  =IPARG(5,NG)            
             IF(ITYP/=1 .AND. ITYP/=2)CYCLE 
             GBUF => ELBUF_TAB(NG)%GBUF
             IF(GBUF%G_SIG > 0)THEN    !this may not be allocated (example : /MAT/VOID)             
              DO I=1,NEL
                P = GBUF%SIG(NEL*(1-1)+I)+GBUF%SIG(NEL*(2-1)+I)+GBUF%SIG(NEL*(3-1)+I)   
                P = -P*THIRD   
                WEIGHT = GBUF%VOL(I)                            
                DO J=2,NNOD+1
                  JJ=IX(J,NFT+I)
                  IS_WRITTEN_NODE(JJ)=1
                  WA4(JJ)=WA4(JJ)+WEIGHT*P                 
                  SUM_WEIGHT(JJ) = SUM_WEIGHT(JJ) + WEIGHT !cumulated volume                 
                ENDDO            
              ENDDO!next I
             END IF
           ENDDO           

         
         ELSEIF(INT22>0)THEN 
         !---------------------------------------------------------!         
         !      /INTER/TYPE22                                      !
         !        specific case due to generic polyhedra           !
         !---------------------------------------------------------!       
         !1. TAG FOR CUT CELLS                                     !
         !2. COMPUTE NODAL PRESSURE                                !
         !     NOT INTERSECTED : NODAL P COMPUTED FROM GLOBAL BUF  !
         !         INTERSECTED : NODAL P COMPUTED FROM SUBVOLUME   !
         !---------------------------------------------------------!                 
           !---1. TAG FOR INTERSECTED BRICKS---!
           !NBF = 1+ITASK*NB/NTHREAD
           !NBL = (ITASK+1)*NB/NTHREAD
           NBF = 1
           NBL = NB
           NIN = 1
           !---1. COMPUTE NODAL PRESSURE---!                  
           DO NG = 1, NGROUP
             NEL   =IPARG(2,NG)
             NFT   =IPARG(3,NG)
             ITYP  =IPARG(5,NG)
             IALEL =IPARG(7,NG)+IPARG(11,NG)   
             GBUF => ELBUF_TAB(NG)%GBUF                        
             IF(ITYP/=1 .AND. ITYP/=2)CYCLE 
             IF(IALEL==0)CYCLE         
             IF(GBUF%G_SIG==0)CYCLE     
              DO I=1,NEL
                IB = NINT(GBUF%TAG22(I))
                !---------------------------!                
                ! NOT A CUT CELL            !
                !---------------------------! 
                IF(IB>0)THEN
                  IF(BRICK_LIST(NIN,IB)%NBCUT==0)IB=0 !in cut cell buffer but not partitioned (because it is adjacent to a cut cell)
                ENDIF             
                IF(IB==0)THEN                
                  P = GBUF%SIG(NEL*(1-1)+I)+GBUF%SIG(NEL*(2-1)+I)+GBUF%SIG(NEL*(3-1)+I)   
                  P = -P*THIRD   
                  WEIGHT = GBUF%VOL(I)                           
                  DO J=2,NNOD+1
                    JJ=IX(J,NFT+I)
                    IS_WRITTEN_NODE(JJ)=1
                    WA4(JJ)=WA4(JJ)+ P*WEIGHT
                    SUM_WEIGHT(JJ) = SUM_WEIGHT(JJ) + WEIGHT !cumulated volume 
                  ENDDO            
                !---------------------------!
                !        CUT CELL           !
                !---------------------------!                                
                ELSE 
                  NIN   = 1 
                  IB    = NINT(GBUF%TAG22(I))
                  MCELL = BRICK_LIST(NIN,IB)%MainID
                  NEL   = IPARG(2,NG)
                  DO J=2,NNOD+1
                    JJ=IX(J,NFT+I)
                    IS_WRITTEN_NODE(JJ)=1
                    ICELL=BRICK_LIST(NIN,IB)%NODE(J-1)%WhichCell
                    IF(ICELL == MCELL)THEN
                      P  = GBUF%SIG(NEL*(1-1)+I)+GBUF%SIG(NEL*(2-1)+I)+GBUF%SIG(NEL*(3-1)+I)   
                      P  = -P*THIRD   
                      WEIGHT  = GBUF%VOL(I)                      
                    ELSE
                      pAdjBRICK  => BRICK_LIST(NIN,IB)%Adjacent_Brick(1:6,1:5)       
                      IPOS       =  BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(1)                    
                      IF(IPOS<=NV46)THEN
                        IV     =  BRICK_LIST(NIN,IB)%Adjacent_Brick(IPOS,1)
                        NGv    =  BRICK_LIST(NIN,IB)%Adjacent_Brick(IPOS,2)
                        IDLOCv =  BRICK_LIST(NIN,IB)%Adjacent_Brick(IPOS,3) 
                        NELv   =  IPARG(2,NGv)                      
                      ELSE
                        J1     =  IPOS/10
                        J2     =  MOD(IPOS,10)
                        IBv    =  BRICK_LIST(NIN,IB )%Adjacent_Brick(J1,4)
                        IV     =  BRICK_LIST(NIN,IBv)%Adjacent_Brick(J2,1)
                        NGv    =  BRICK_LIST(NIN,IBv)%Adjacent_Brick(J2,2)
                        IDLOCv =  BRICK_LIST(NIN,IBv)%Adjacent_Brick(J2,3) 
                        NELv   =  IPARG(2,NGv)                                                
                      ENDIF                      
                      GBUFv    => ELBUF_TAB(NGv)%GBUF
                      P        =  GBUFv%SIG(NELv*(1-1)+IDLOCv)+GBUFv%SIG(NELv*(2-1)+IDLOCv)+GBUFv%SIG(NELv*(3-1)+IDLOCv)
                      P        = -P*THIRD   
                      WEIGHT   =  GBUFv%VOL(IDLOCv)                                             
                    ENDIF
                    WA4(JJ)=WA4(JJ)+P*WEIGHT
                    SUM_WEIGHT(JJ) = SUM_WEIGHT(JJ) + WEIGHT
                  ENDDO            
                ENDIF
              ENDDO
           ENDDO                 
         ENDIF
         
C-----------------------------------------------
         !divinding by sum of weights to get finally weighting factors
         DO I=1,NUMNOD
           IF(SUM_WEIGHT(I)/=ZERO)THEN
             WA4(I)=WA4(I)/SUM_WEIGHT(I)                    
           ENDIF
         ENDDO
         
         DEALLOCATE(SUM_WEIGHT)          
C-----------------------------------------------
      
      RETURN
      END
