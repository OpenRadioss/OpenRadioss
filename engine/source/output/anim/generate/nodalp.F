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
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE NODALP(IFUNC    , WA4, IFLOW, RFLOW, IPARG, 
     .                  ELBUF_TAB, IX , NIX  , ITAB , NV46)
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
#include      "scr05_c.inc"
#include      "flowcom.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFUNC, IFLOW(*),IPARG(NPARG,*),IX(NIX,*),ITAB(*),NIX,NV46
      my_real RFLOW(*)
      REAL WA4(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB      
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IADI, IADR, I, ITYP, NINOUT, NNO, NEL, NELv,II1, II2,
     .        IR1, IR2, J, JJ, NNO_L, NNI_L, II3, II4, JJJ, NNI,
     .        IALEL,NNOD,IPOS,IV,NGv,IDLOCv,J1,J2,IBV
      INTEGER MLW, NG, KCVT, II(6), NBF, NBL, IB, ICELL, NIN, MCELL
      TYPE(G_BUFEL_)  ,POINTER :: GBUF,GBUFv     
      my_real, ALLOCATABLE, DIMENSION(:) :: COUNT_VOL    
      my_real P,V
      INTEGER,DIMENSION(:,:), POINTER   :: pAdjBRICK      
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   This subroutine write nodal pressures OR nodal potential in animation files.
C   /BEM/FLOW      (nodal Pressure & nodal Potential).
C   /INTER/TYPE22  (nodal Pressure only).
C
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      

         NNOD = NIX-3   !8-node brick or 4-node quad

         IF(INT22==0)THEN
         !---------------------------------------------------------!         
         !         ALE STANDARD FORMULATION                        !
         !---------------------------------------------------------!       
         !1. COMPUTE NODAL PRESSURE                                !
         !---------------------------------------------------------!         
           !---1. COMPUTE NODAL PRESSURE---!      
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
             IF(GBUF%G_SIG > 0)THEN    !this may not be allocated (example : /MAT/VOID)             
              DO I=1,NEL
                P = GBUF%SIG(NEL*(1-1)+I)+GBUF%SIG(NEL*(2-1)+I)+GBUF%SIG(NEL*(3-1)+I)   
                P = -P*THIRD   
                V = GBUF%VOL(I)                            
                DO J=2,NNOD+1
                  JJ=IX(J,NFT+I)
                  WA4(JJ)=WA4(JJ)+V*P               !cumulated mass
                  COUNT_VOL(JJ) = COUNT_VOL(JJ) + V !cumulated volume                 
                ENDDO            
              ENDDO!next I
             END IF
           ENDDO
           !applying weight factor
           DO I=1,NUMNOD
             IF(COUNT_VOL(I)/=ZERO)THEN
               WA4(I)=WA4(I)/COUNT_VOL(I)
             ENDIF
           ENDDO                 
           DEALLOCATE(COUNT_VOL)             
C------------------------------------------------------------------
C        BEM/FLOW
C------------------------------------------------------------------
           IF(NFLOW > 0) CALL ANINFLOW(IFUNC,WA4,IFLOW,RFLOW)
C
           GOTO 100
         
         ELSEIF(INT22>0)THEN 
         !---------------------------------------------------------!         
         !         /INTER/TYPE22                                   !
         !---------------------------------------------------------!       
         !1. TAG FOR CUT CELLS                                     !
         !2. COMPUTE NODAL PRESSURE                                !
         !     NOT INTERSECTED : NODAL P COMPUTED FROM GLOBAL BUF  !
         !         INTERSECTED : NODAL P COMPUTED FROM SUBVOLUME   !
         !---------------------------------------------------------!                 
           ALLOCATE(COUNT_VOL(NUMNOD))      
           COUNT_VOL = 0
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
C                print *, "treating brick id=", IX(11,I+NFT)
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
                  V = GBUF%VOL(I)                           
                  DO J=2,NNOD+1
                    JJ=IX(J,NFT+I)
                    WA4(JJ)=WA4(JJ)+ P*V              !cumulated mass
                    COUNT_VOL(JJ) = COUNT_VOL(JJ) + V !cumulated volume 
c                   if(ibug22_nodalp==1)then                    
c                       print *, "adding on node", itab(JJ)
c                       print *, "    P=", P
c                       print *, "    V=", V 
c                       print *, "   WA=", WA4(JJ)
c                       print *, " sumV=", COUNT_VOL(JJ)
c                   endif
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
                    ICELL=BRICK_LIST(NIN,IB)%NODE(J-1)%WhichCell
                    IF(ICELL == MCELL)THEN
                      P  = GBUF%SIG(NEL*(1-1)+I)+GBUF%SIG(NEL*(2-1)+I)+GBUF%SIG(NEL*(3-1)+I)   
                      P  = -P*THIRD   
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
                      P          =  GBUFv%SIG(NELv*(1-1)+IDLOCv)+
     .                              GBUFv%SIG(NELv*(2-1)+IDLOCv)+
     .                              GBUFv%SIG(NELv*(3-1)+IDLOCv)
                      P          = -P*THIRD   
                      V          =  GBUFv%VOL(IDLOCv)                                             
                    ENDIF
                    WA4(JJ)=WA4(JJ)+P*V
                    COUNT_VOL(JJ) = COUNT_VOL(JJ) + V
c                   if(ibug22_nodalp==1)then                    
c                       print *, "adding on node", itab(JJ)
c                       print *, "    P=", P
c                       print *, "    V=", V 
c                       print *, "   WA=", WA4(JJ)
c                       print *, " sumV=", COUNT_VOL(JJ)
c                   endif
                  ENDDO            
                ENDIF
              ENDDO
           ENDDO
           !applying weight factor
           DO I=1,NUMNOD
             IF(COUNT_VOL(I)/=ZERO)THEN
               WA4(I)=WA4(I)/COUNT_VOL(I)
c                if(ibug22_nodalp==1)then               
c                    print *, "normilze node", itab(I)
c                    print *, "  sum V=",COUNT_VOL(I) 
c                    print *, "     WA=", WA4(I)
c               endif
                    
             ENDIF
           ENDDO
           DEALLOCATE(COUNT_VOL)                  
           GOTO 100
         ENDIF
                  
  100 CONTINUE        
      RETURN
      END
