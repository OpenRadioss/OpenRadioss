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
Chd|  NODALT                        source/output/anim/generate/nodalt.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        H3D_NODAL_SCALAR              source/output/h3d/h3d_results/h3d_nodal_scalar.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE NODALT(IFUNC    , WA4      , WA4_FVM, IFLOW  , RFLOW,  
     .                  IPARG    , ELBUF_TAB, IX     , NIX    , NUMEL, 
     .                  ITAB     , NV46     , MONVOL , VOLMON , AIRBAGS_TOTAL_FVM_IN_H3D,
     .                  IS_WRITTEN_NODE,IS_WRITTEN_NODE_FVM, ISPMD, FVDATA_P, SWA4)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   This subroutine defines & writes nodal temperature requested by Engine keywords /H3D/NODA/TEMP or /ANIMNODA/TEMP
C      application :  /MONVOL/FVMBAG* (temperature from polyhedra, specific buffer).
C                     ELEMS (values expanded to adjacent nodes)
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
      USE FVBAG_MOD , only:FVBAG_DATA,NFVBAG  
      USE GROUPDEF_MOD , only:GROUP_                
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
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NUMEL, IFUNC, NIX, NV46,ITAB(NUMNOD), ISPMD, SWA4
      INTEGER,INTENT(IN) :: IFLOW(*),IPARG(NPARG,NGROUP),IX(NIX,NUMEL)
      my_real,INTENT(IN) :: RFLOW(*)
      INTEGER,INTENT(IN) :: AIRBAGS_TOTAL_FVM_IN_H3D
      REAL,INTENT(INOUT) :: WA4(SWA4),WA4_FVM(AIRBAGS_TOTAL_FVM_IN_H3D)
      TYPE (ELBUF_STRUCT_), INTENT(IN), DIMENSION(NGROUP), TARGET :: ELBUF_TAB    
      INTEGER,INTENT(IN) :: MONVOL(SMONVOL) 
      my_real,INTENT(IN) :: VOLMON(SVOLMON)
      INTEGER,INTENT(INOUT) :: IS_WRITTEN_NODE_FVM(AIRBAGS_TOTAL_FVM_IN_H3D),IS_WRITTEN_NODE(NUMNOD)
      TYPE(FVBAG_DATA),INTENT(IN) :: FVDATA_P(NFVBAG)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IADI, IADR, I, ITYP, NINOUT, NNO, NEL, NELv,II1, II2,
     .        IR1, IR2, J, JJ, NNO_L, NNI_L, II3, II4, JJJ, NNI,
     .        IALEL,NNOD,IPOS,IV,NGv,IDLOCv,J1,J2,IBV
      INTEGER MLW, NG, KCVT, II(6), NBF, NBL, IB, ICELL, NIN, MCELL
      TYPE(G_BUFEL_)  ,POINTER :: GBUF,GBUFv     
      my_real, ALLOCATABLE, DIMENSION(:) :: COUNT_VOL    
      my_real V,T
      INTEGER,DIMENSION(:,:), POINTER   :: pAdjBRICK      
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      

         NNOD = NIX-3   !8-node brick or 4-node quad

         IF(INT22==0)THEN
C------------------------------------------------------------------
C        DEFAULT - ALL ELEMS (HEXA,PENTA, SHELL, QUAD, ..)
C           expand elem temperature to its nodes
C------------------------------------------------------------------      
           ALLOCATE(COUNT_VOL(NUMNOD))                 
           COUNT_VOL(:) = 0                           
           DO NG = 1, NGROUP
             NEL   =IPARG(2,NG)
             NFT   =IPARG(3,NG)
             ITYP  =IPARG(5,NG)
             IALEL =IPARG(7,NG)+IPARG(11,NG)             
             IF(ITYP/=1 .AND. ITYP/=2)CYCLE 
             !IF(IALEL==0)CYCLE
             GBUF => ELBUF_TAB(NG)%GBUF
             DO I=1,NEL
                T = GBUF%TEMP(I)
                V = GBUF%VOL(I)                            
                DO J=2,NNOD+1
                  JJ=IX(J,NFT+I)
                  WA4(JJ)=WA4(JJ)+V*T               !cumulated quantity
                  COUNT_VOL(JJ) = COUNT_VOL(JJ) + V !cumulated volume  
                  IS_WRITTEN_NODE(JJ)=1               
                ENDDO            
             ENDDO
           ENDDO
           !applying weight factor
           DO I=1,NUMNOD
             IF(COUNT_VOL(I)/=ZERO)THEN
               WA4(I)=WA4(I)/COUNT_VOL(I)
             ENDIF
           ENDDO                 
           DEALLOCATE(COUNT_VOL)             
         
C------------------------------------------------------------------
C        CUT CELLS (INTER TYPE 22)
C------------------------------------------------------------------      
         ELSEIF(INT22>0)THEN 
         !---------------------------------------------------------!         
         !         /INTER/TYPE22                                   !
         !---------------------------------------------------------!       
         !1. TAG FOR CUT CELLS                                     !
         !2. COMPUTE NODAL TEMPERATURE                             !
         !     NOT INTERSECTED : NODAL T COMPUTED FROM GLOBAL BUF  !
         !         INTERSECTED : NODAL T COMPUTED FROM SUBVOLUME   !
         !---------------------------------------------------------!                 
           ALLOCATE(COUNT_VOL(NUMNOD))      
           COUNT_VOL = 0
           NBF = 1
           NBL = NB
           NIN = 1
           !---1. COMPUTE NODAL PRESSURE---!                  
           DO NG = 1, NGROUP
             NEL   =IPARG(2,NG)
             NFT   =IPARG(3,NG)
             ITYP  =IPARG(5,NG)
             IALEL =IPARG(7,NG)+IPARG(11,NG)             
             IF(ITYP/=1 .AND. ITYP/=2)CYCLE 
             IF(IALEL==0)CYCLE             
             GBUF => ELBUF_TAB(NG)%GBUF 
              DO I=1,NEL
                IB = NINT(GBUF%TAG22(I))
                !---------------------------!                
                ! NOT A CUT CELL            !
                !---------------------------! 
                IF(IB>0)THEN
                  IF(BRICK_LIST(NIN,IB)%NBCUT==0)IB=0 !in cut cell buffer but not partitioned (because it is adjacent to a cut cell)
                ENDIF             
                IF(IB==0)THEN                
                  T = GBUF%TEMP(I)     
                  V = GBUF%VOL(I)                           
                  DO J=2,NNOD+1
                    JJ=IX(J,NFT+I)
                    WA4(JJ)=WA4(JJ)+ T*V              !cumulated quantity
                    COUNT_VOL(JJ) = COUNT_VOL(JJ) + V !cumulated volume 
                    IS_WRITTEN_NODE(JJ)=1
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
                      T  = GBUF%TEMP(I) 
                      V  = GBUF%VOL(I)                      
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
                      GBUFv      => ELBUF_TAB(NGv)%GBUF
                      T          =  GBUFv%TEMP(IDLOCv)
                      V          =  GBUFv%VOL(IDLOCv)                                             
                    ENDIF
                    WA4(JJ)=WA4(JJ)+T*V
                    COUNT_VOL(JJ) = COUNT_VOL(JJ) + V
                  ENDDO            
                ENDIF
              ENDDO
           ENDDO
           !applying weight factor
           DO I=1,NUMNOD
             IF(COUNT_VOL(I)/=ZERO)THEN
               WA4(I)=WA4(I)/COUNT_VOL(I)                    
             ENDIF
           ENDDO
           DEALLOCATE(COUNT_VOL)                  
         ENDIF
                  
C------------------------------------------------------------------
C        /MONVOL/FVMBAG*
C           set temperature from polyhedra centroids
C------------------------------------------------------------------
        IF(NFVBAG > 0 .AND. ISPMD == 0 .AND. AIRBAGS_TOTAL_FVM_IN_H3D > 0)THEN
          CALL ANIM_NODAL_CONTOUR_FVMBAGS('TEMP', WA4_FVM, MONVOL , VOLMON                  , FVDATA_P           , 
     .                                    NFVBAG, SMONVOL, SVOLMON, AIRBAGS_TOTAL_FVM_IN_H3D, IS_WRITTEN_NODE_FVM)
        
        ENDIF


      RETURN
      END
