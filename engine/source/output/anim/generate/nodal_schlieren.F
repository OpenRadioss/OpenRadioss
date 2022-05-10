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
Chd|  NODAL_SCHLIEREN               source/output/anim/generate/nodal_schlieren.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        H3D_NODAL_SCALAR              source/output/h3d/h3d_results/h3d_nodal_scalar.F
Chd|-- calls ---------------
Chd|        AGRAD3                        source/ale/ale3d/agrad3.F     
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE NODAL_SCHLIEREN(WA4, X,IXS, IXQ, ITAB,IPARG ,IBID,ELBUF_TAB,ALE_CONNECTIVITY)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     This subroutine outputs data for schlieren.
C schlieren is eta = exp (-C ||grad(rho)||)
C C is a constant which help user to adjust "brightness"
C RADIOSS outputs density gradient which is recuired to output schlieren.
C 'C' cosntant must be tuned during post-treatment then
C is it introduced with HV result math.
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
C     IALEL > 0 
C        where IALEL =IPARG(7,NG)+IPARG(11,NG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INITBUF_MOD
      USE ELBUFDEF_MOD  
      USE I22BUFBRIC_MOD   
      USE I22EDGE_MOD    
      USE I22TRI_MOD           
      USE ALE_CONNECTIVITY_MOD
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
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXQ(NIXQ,*),IXS(NIXS,*),ITAB(*),IPARG(NPARG,*)
      REAL WA4(*)
      my_real :: X(3,*)
      INTEGER :: IBID
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB            
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IADI, IADR, I, ITYP, NINOUT, NNO, NEL, II1, II2,
     .        IR1, IR2, J, JJ, NNO_L, NNI_L, II3, II4, JJJ, NNI,
     .        IALEL,NNOD,IPOS,NGv,IDLOCv,K, IAD2
      INTEGER IV(6), IE
      INTEGER MLW, NG, KCVT, II, NBF, NBL, IB, ICELL, NIN, MCELL
      TYPE(G_BUFEL_)  ,POINTER :: GBUF,GBUFv     
      my_real, ALLOCATABLE, DIMENSION(:) :: COUNT_VOL    
      my_real D,V, DPHI(MVSIZ)
      INTEGER,DIMENSION(:,:), POINTER   :: pAdjBRICK 
      my_real :: GRAD(6,MVSIZ)     
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C   This subroutine write nodal shadowgraph (schlieren)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      

         WA4(1:NUMNOD) = ZERO

         RETURN

         !TODO : 2D,3D, QUAD, HEXA,TETRA





         NNOD = NIXS-3   !8-node brick or 4-node quad

         !---------------------------------------------------------!         
         !         ALE STANDARD FORMULATION  : 3D                  !
         !---------------------------------------------------------!       
           ALLOCATE(COUNT_VOL(NUMNOD))                 
           COUNT_VOL(:) = 0                           
           DO NG = 1, NGROUP
             NEL   =IPARG(2,NG)
             NFT   =IPARG(3,NG)
             ITYP  =IPARG(5,NG)
             IALEL =IPARG(7,NG)+IPARG(11,NG)             
             IF(ITYP/=1 .AND. ITYP/=2)CYCLE 
             IF(IALEL==0)CYCLE
              GBUF => ELBUF_TAB(NG)%GBUF   
              DO I=1,NEL
                J = I+NFT
!                PHI(J) = GBUF%RHO(I)  
              ENDDO
           ENDDO

           DO NG = 1, NGROUP
             NEL   =IPARG(2,NG)
             NFT   =IPARG(3,NG)
             ITYP  =IPARG(5,NG)
             IALEL =IPARG(7,NG)+IPARG(11,NG)             
             IF(ITYP/=1 .AND. ITYP/=2)CYCLE 
             IF(IALEL==0)CYCLE
              GBUF => ELBUF_TAB(NG)%GBUF   
              DO I=1,NEL
                LFT = 1
                LLT = NEL
                CALL AGRAD3(IXS,X,ALE_CONNECTIVITY,GRAD)
                IE =NFT+I
                IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
                IV(1)=ALE_CONNECTIVITY%ee_connect%connected(IAD2 + 1 - 1)
                IV(2)=ALE_CONNECTIVITY%ee_connect%connected(IAD2 + 2 - 1)
                IV(3)=ALE_CONNECTIVITY%ee_connect%connected(IAD2 + 3 - 1)
                IV(4)=ALE_CONNECTIVITY%ee_connect%connected(IAD2 + 4 - 1)
                IV(5)=ALE_CONNECTIVITY%ee_connect%connected(IAD2 + 5 - 1)
                IV(6)=ALE_CONNECTIVITY%ee_connect%connected(IAD2 + 6 - 1)
                IF(IV(1).LE.0)IV(1)=IE
                IF(IV(2).LE.0)IV(2)=IE
                IF(IV(3).LE.0)IV(3)=IE
                IF(IV(4).LE.0)IV(4)=IE
                IF(IV(5).LE.0)IV(5)=IE
                IF(IV(6).LE.0)IV(6)=IE
                DPHI(I) = ZERO
!     .           (PHI(IV(1))-PHI(IE))*GRAD(1,I) 
!     .          +(PHI(IV(2))-PHI(IE))*GRAD(2,I) 
!     .          +(PHI(IV(3))-PHI(IE))*GRAD(3,I) 
!     .          +(PHI(IV(4))-PHI(IE))*GRAD(4,I) 
!     .          +(PHI(IV(5))-PHI(IE))*GRAD(5,I) 
!     .          +(PHI(IV(6))-PHI(IE))*GRAD(6,I)  
                  DO J=2,NNOD+1
                    JJ=IXS(J,NFT+I)
                    K = J-1
                    WA4(JJ)       = WA4(JJ)+ DPHI(I) 
                    COUNT_VOL(JJ) = COUNT_VOL(JJ) + 1        
                  ENDDO
              ENDDO
           ENDDO!next NG

           !applying weight factor
           DO I=1,NUMNOD
             IF(COUNT_VOL(I)/=ZERO)THEN
               WA4(I)=WA4(I)/COUNT_VOL(I)
             ENDIF
           ENDDO                 
           DEALLOCATE(COUNT_VOL)             

           END
        
