Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  NODALZVOL                     source/output/anim/generate/nodalzvol.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE NODALZVOL (IFUNC    , WA4, IFLOW, RFLOW, IPARG, 
     .                       ELBUF_TAB, IX , NIX  , ITAB , NV46 )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     This suroutine display polyhedra volume (old & new)
C   using free node as marker (inter22 only, see input card for grnod_id)
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
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IFUNC, IFLOW(*),IPARG(NPARG,*),IX(NIX,*),ITAB(*),NIX,NV46
      my_real,INTENT(IN) :: RFLOW(*)
      REAL,INTENT(INOUT) :: WA4(*)
      
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER                            :: IADI, IADR, I, ITYP, NINOUT, NNO, NEL, II1, II2,K1,K,
     .                                      IR1, IR2, J, JJ, NNO_L, NNI_L, II3, II4, JJJ, NNI,
     .                                      IALEL,NNOD,IPOS,IV,NGv,J1,J2,IBV, MLW,NumNodCell,
     .                                      NG, KCVT, II, NBF, NBL, IB, ICELL, NIN, MCELL,NCELL,
     .                                      IPHASE,NODE_ID,INEW
      TYPE(G_BUFEL_)  ,POINTER           :: GBUF,GBUFv     
      my_real, ALLOCATABLE, DIMENSION(:) :: COUNT_VOL    
      my_real                            :: P,VF,D,V
      INTEGER,DIMENSION(:,:), POINTER    :: pAdjBRICK      
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   This subroutine writes nodal VFRAC
C   /INTER/TYPE22  (only).
C-----------------------------------------------
C   P r e c o n d i t i o n s
C-----------------------------------------------
         IF(INT22==0)RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      

         NNOD   = NIX-3   !8-node brick or 4-node quad
         INEW = 29-IFUNC  ! 1: new cell volume, 0:old 

         !---------------------------------------------------------!         
         !         /INTER/TYPE22                                   !
         !---------------------------------------------------------!       
         !   CELL CENTROIDS ARE MARKED IN CUT CELL BUFFER          !
         !   PURPOSE IS TO DISPLAY CELL SCALR USING FREE NODE      !
         !---------------------------------------------------------!                 
         !---1. TAG FOR INTERSECTED BRICKS---!                                                                                                         
         NBF = 1                                                                                                                                       
         NBL = NB                                                                                                                                      
         NIN = 1                                                                                                                                       
         !---1. COMPUTE NODAL PRESSURE---!                                                                                                             
         DO NG = 1, NGROUP                                                                                                                             
           MLW   = IPARG(1,NG)                                                                                                                         
           NEL   = IPARG(2,NG)                                                                                                                         
           NFT   = IPARG(3,NG)                                                                                                                         
           ITYP  = IPARG(5,NG)                                                                                                                         
           IALEL = IPARG(7,NG)+IPARG(11,NG)                                                                                                            
           IF(ITYP/=1 .AND. ITYP/=2)CYCLE                                                                                                              
           IF(IALEL==0)CYCLE                                                                                                                           
           IF(MLW/=6 .AND. MLW/=11 .AND. MLW/=37.AND.MLW/=51)CYCLE                                                                                                                
           GBUF => ELBUF_TAB(NG)%GBUF                                                                                                                  
           NIN  = 1                                                                                                                                    
           DO I=1,NEL                                                                                                                                  
             IB = NINT(GBUF%TAG22(I))                                                                                                                  
             !---------------------------!                                                                                                             
             ! NOT A CUT CELL            !                                                                                                             
             !---------------------------!                                                                                                             
             IF(IB==0)CYCLE                                                                                                                            
             !---------------------------!                                                                                                             
             !        CUT CELL           !                                                                                                             
             !---------------------------!                                                                                                             
             ICELL   =  0  
             NCELL   = BRICK_LIST(NIN,IB)%NBCUT                                                                                                              
             DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}                                                                            
               ICELL = ICELL +1                                                                                                                        
               IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9                                                                                                  
               NODE_ID      = BRICK_LIST(NIN,IB)%POLY(ICELL)%ID_FREE_NODE  
               IF(NODE_ID<=0)EXIT ! no enough nodes in the group or debug option not compatible with smp
               IF(INEW == 1)THEN                                                                                
                 WA4(NODE_ID) = BRICK_LIST(NIN,IB)%POLY(ICELL)%VNEW                                                                                
               ELSE
                 WA4(NODE_ID) = BRICK_LIST(NIN,IB)%POLY(ICELL)%VOLD                                                                                                
               ENDIF
             ENDDO                                                                                                                                      
           ENDDO!next I                                                                                                                                
         ENDDO!next NG                                                                                                                                 
                  

C-----------------------------------------------         
      RETURN
      END
