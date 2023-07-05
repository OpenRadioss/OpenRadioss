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
Chd|  H3D_VELVECC22                 source/output/h3d/h3d_results/h3d_velvecc22.F
Chd|-- called by -----------
Chd|        H3D_NODAL_VECTOR              source/output/h3d/h3d_results/h3d_nodal_vector.F
Chd|-- calls ---------------
Chd|        H3D_WRITE_VECTOR              source/output/h3d/h3d_results/h3d_write_vector.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        I22TRI_MOD                    ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE H3D_VELVECC22(ELBUF_TAB,IPARG,IFLG,IXS,IXQ,ITAB,
     .                         IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C This subroutines writes at polyedra centroids :
C     velocities       (IFLG=1),
C     momentum density (IFLG=2)
C     internal forces  (IFLG=3),
C for coupling interface 22. Free nodes are used 
C as marker to plot centroid vectors 
C(see input card for grnod_id)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: IPARG(NPARG,*), IFLG,IXS(NIXS,NUMELS),IXQ(NIXQ,NUMELQ),ITAB(NUMNOD)
      INTEGER, INTENT(IN)    :: IOK_PART(*),IS_WRITTEN_NODE(*) 
      my_real, INTENT(INOUT) :: NODAL_VECTOR(3,*)   
      REAL R4
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET :: ELBUF_TAB      
      TYPE(BUF_MAT_),POINTER                          :: MBUF   
      TYPE(G_BUFEL_),POINTER                          :: GBUF 
C-----------------------------------------------
C   L o c a l   A r g u m e n t s
C----------------------------------------------- 
      INTEGER :: NGM, IDLOCM, IBM,ICELLM,MLW,NCELL,NELm,NBF,NBL,ICELL,NIN,NODE_ID,IB,NG,I
      my_real :: rho_cell, RHO(4), VFRAC(4)
      REAL,DIMENSION(:,:),ALLOCATABLE :: BUFFER
      my_real VALUE(3)
C-----------------------------------------------

       !---------------------------------------------------------!                 
       NBF = 1                                                                                                                                       
       NBL = NB                                                                                                                                      
       NIN = 1              
       !---------------------------------------------------------!  
       ALLOCATE(BUFFER(3,NUMNOD))
       BUFFER(:,:)           = ZERO  
             
       DO IB=NBF,NBL    
         ICELL               =  0                                                                
         NCELL               = BRICK_LIST(NIN,IB)%NBCUT                                                                                                          
         DO WHILE (ICELL<=NCELL) ! loop on polyhedron {1:NCELL} U {9}                                                                            
           ICELL             = ICELL +1                                                                                                                        
           IF (ICELL>NCELL .AND. NCELL/=0)ICELL=9   
           IBM               = BRICK_LIST(NIN,IB)%POLY(ICELL)%WhereIsMain(4)
           ICELLM            = BRICK_LIST(NIN,IBM)%mainID
           IF(IBM==0)THEN
             IBM             = IB                                                         
             ICELLM          = 1                                                          
           ENDIF
           NGM               = BRICK_LIST(NIN,IBM)%NG                                     
           IDLOCM            = BRICK_LIST(NIN,IBM)%IDLOC                                  
           GBUF              =>ELBUF_TAB(NGM)%GBUF   
           MBUF              =>ELBUF_TAB(NGM)%BUFLY(1)%MAT(1,1,1)                                                                                                            
           NELm              = IPARG(2,NGM)   
           MLW               = IPARG(1,NGM)                                             
           IF(MLW==37)THEN
              !UVAR(I,1) : massic percentage of liquid * global density  (rho1*V1/V : it needs to give liquid mass multiplying by element volume in aleconve.F)
              !UVAR(I,2) : density of gas
              !UVAR(I,3) : density of liquid
              !UVAR(I,4) : volumetric fraction of liquid
              !UVAR(I,5) : volumetric fraction of gas               
              RHO(1)         = MBUF%VAR((3-1)*NELm+IDLOCM)
              RHO(2)         = MBUF%VAR((2-1)*NELm+IDLOCM)  
              VFRAC(1)       = MBUF%VAR((4-1)*NELm+IDLOCM)
              VFRAC(2)       = MBUF%VAR((5-1)*NELm+IDLOCM)            
              rho_cell       = RHO(1)*VFRAC(1) + RHO(2)*VFRAC(2)
           ELSEIF(MLW==51)THEN
              RHO(1)         = ZERO
              RHO(2)         = ZERO                 
              rho_cell       = ZERO              
           ELSE 
              rho_cell       = GBUF%RHO(IDLOCM)
           ENDIF
           NODE_ID           = BRICK_LIST(NIN,IB)%POLY(ICELL)%ID_FREE_NODE  
           IF(NODE_ID<=0)CYCLE ! not enough nodes in the group or SMP disabling
           IF(IFLG==1)THEN       
             !velocity vector : U                                                          
             BUFFER(1,NODE_ID) = GBUF%MOM(NELm*(1-1) + IDLOCm) / rho_cell                                                                               
             BUFFER(2,NODE_ID) = GBUF%MOM(NELm*(2-1) + IDLOCm) / rho_cell 
             BUFFER(3,NODE_ID) = GBUF%MOM(NELm*(3-1) + IDLOCm) / rho_cell                      
           ELSEIF(IFLG==2)THEN
             !momentum density vector : rho.U
             BUFFER(1,NODE_ID) = GBUF%MOM(NELm*(1-1) + IDLOCm) 
             BUFFER(2,NODE_ID) = GBUF%MOM(NELm*(2-1) + IDLOCm) 
             BUFFER(3,NODE_ID) = GBUF%MOM(NELm*(3-1) + IDLOCm)
           ELSEIF(IFLG==3)THEN   
             !internal force at centroid = sum(integral(P.dS))
             BUFFER(1,NODE_ID) = BRICK_LIST(NIN,IBM)%FCELL(1)
             BUFFER(2,NODE_ID) = BRICK_LIST(NIN,IBM)%FCELL(2)
             BUFFER(3,NODE_ID) = BRICK_LIST(NIN,IBM)%FCELL(3)
           ELSE
             BUFFER(1,NODE_ID) = ZERO
             BUFFER(2,NODE_ID) = ZERO
             BUFFER(3,NODE_ID) = ZERO                          
           ENDIF
         ENDDO !next ICELL          
       ENDDO!next IB
        
       DO I=1,NUMNOD   
	 VALUE(1)=BUFFER(1,I)
	 VALUE(2)=BUFFER(2,I)
	 VALUE(3)=BUFFER(3,I)
         CALL H3D_WRITE_VECTOR(IOK_PART,IS_WRITTEN_NODE,NODAL_VECTOR,I,0,0,
     .   				 VALUE)
       ENDDO
          
       DEALLOCATE(BUFFER)   
      !---------------------------------------------------------! 

      RETURN
      END
