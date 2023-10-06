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
Chd|  OUTPUT_DIV_U                  source/output/anim/generate/output_div_u.F
Chd|-- called by -----------
Chd|        H3D_QUAD_SCALAR               source/output/h3d/h3d_results/h3d_quad_scalar.F
Chd|        H3D_SHELL_SCALAR_1            source/output/h3d/h3d_results/h3d_shell_scalar_1.F
Chd|        H3D_SOLID_SCALAR_1            source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE OUTPUT_DIV_U( EVAR,IX,X,V,IPARG,ELBUF_TAB,NG, NIX,ITYP,
     .                         NUMEL, NEL, NUMNOD, NPARG, NGROUP, N2D , NFT)
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     This subroutine outputs div(v) : volumetric dilatation rate.
C-----------------------------------------------
C   P r e - C o n d i t i o n s
C-----------------------------------------------
C     none
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: NUMEL,NEL,NUMNOD,NPARG,NGROUP,N2D,NFT,ITYP
      INTEGER,INTENT(IN) :: IX(NIX,NUMEL),IPARG(NPARG,NGROUP),NIX,NG
      my_real,INTENT(IN) :: X(3,NUMNOD),V(3,NUMNOD)
      my_real,INTENT(OUT) :: EVAR(NEL)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP), INTENT(IN), TARGET :: ELBUF_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MLW, IE
      my_real :: DIV , Xi(8), Yi(8), Zi(8)
      INTEGER :: NC(8)
      my_real :: N(3,6), VEL(6,3),  AREA, NX, A1, A2, DOTPROD(6,NEL), VOL(NEL)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      

      MLW  = IPARG(01,NG)
      
      DO I=1,NEL
        DOTPROD(1,I)=ZERO
        DOTPROD(2,I)=ZERO
        DOTPROD(3,I)=ZERO
        DOTPROD(4,I)=ZERO
        DOTPROD(5,I)=ZERO
        DOTPROD(6,I)=ZERO
      ENDDO

      !-------------------------------------!                                                                                   
      !     COMPUTE NORMAL ON FACES S.n
      !-------------------------------------!   
      IF(N2D == 0 .AND. ITYP == 1)THEN !3d solids
        DO I=1,NEL
          IE=I+NFT      
         !---8 local node numbers NC1 TO NC8 for 3D solid element IE ---!         
          NC(1)=IX(2,IE)                                                     
          NC(2)=IX(3,IE)                                                     
          NC(3)=IX(4,IE)                                                     
          NC(4)=IX(5,IE)                                                     
          NC(5)=IX(6,IE)                                                     
          NC(6)=IX(7,IE)                                                     
          NC(7)=IX(8,IE)                                                     
          NC(8)=IX(9,IE)                                                     
          !---Coordinates of the 8 nodes                                       
          Xi(1)=X(1,NC(1))                                                    
          Yi(1)=X(2,NC(1))                                                    
          Zi(1)=X(3,NC(1))                                                    
          !                                                              
          Xi(2)=X(1,NC(2))                                                    
          Yi(2)=X(2,NC(2))                                                    
          Zi(2)=X(3,NC(2))                                                    
          !                                                              
          Xi(3)=X(1,NC(3))                                                    
          Yi(3)=X(2,NC(3))                                                    
          Zi(3)=X(3,NC(3))                                                    
          !                                                              
          Xi(4)=X(1,NC(4))                                                    
          Yi(4)=X(2,NC(4))                                                    
          Zi(4)=X(3,NC(4))                                                    
          !                                                              
          Xi(5)=X(1,NC(5))                                                    
          Yi(5)=X(2,NC(5))                                                    
          Zi(5)=X(3,NC(5))                                                    
          !                                                              
          Xi(6)=X(1,NC(6))                                                    
          Yi(6)=X(2,NC(6))                                                    
          Zi(6)=X(3,NC(6))                                                    
          !                                                              
          Xi(7)=X(1,NC(7))                                                    
          Yi(7)=X(2,NC(7))                                                    
          Zi(7)=X(3,NC(7))                                                    
          !                                                              
          Xi(8)=X(1,NC(8))                                                    
          Yi(8)=X(2,NC(8))                                                    
          Zi(8)=X(3,NC(8)) 
           ! Face)-1
          N(1,1)=(Yi(3)-Yi(1))*(Zi(2)-Zi(4)) - (Zi(3)-Zi(1))*(Yi(2)-Yi(4))     
          N(2,1)=(Zi(3)-Zi(1))*(Xi(2)-Xi(4)) - (Xi(3)-Xi(1))*(Zi(2)-Zi(4))     
          N(3,1)=(Xi(3)-Xi(1))*(Yi(2)-Yi(4)) - (Yi(3)-Yi(1))*(Xi(2)-Xi(4))     
          ! Face)-2                                                            
          N(1,2)=(Yi(7)-Yi(4))*(Zi(3)-Zi(8)) - (Zi(7)-Zi(4))*(Yi(3)-Yi(8))     
          N(2,2)=(Zi(7)-Zi(4))*(Xi(3)-Xi(8)) - (Xi(7)-Xi(4))*(Zi(3)-Zi(8))     
          N(3,2)=(Xi(7)-Xi(4))*(Yi(3)-Yi(8)) - (Yi(7)-Yi(4))*(Xi(3)-Xi(8))     
          ! Face)-3                                                            
          N(1,3)=(Yi(6)-Yi(8))*(Zi(7)-Zi(5)) - (Zi(6)-Zi(8))*(Yi(7)-Yi(5))     
          N(2,3)=(Zi(6)-Zi(8))*(Xi(7)-Xi(5)) - (Xi(6)-Xi(8))*(Zi(7)-Zi(5))     
          N(3,3)=(Xi(6)-Xi(8))*(Yi(7)-Yi(5)) - (Yi(6)-Yi(8))*(Xi(7)-Xi(5))     
          ! Face)-4                                                            
          N(1,4)=(Yi(2)-Yi(5))*(Zi(6)-Zi(1)) - (Zi(2)-Zi(5))*(Yi(6)-Yi(1))     
          N(2,4)=(Zi(2)-Zi(5))*(Xi(6)-Xi(1)) - (Xi(2)-Xi(5))*(Zi(6)-Zi(1))     
          N(3,4)=(Xi(2)-Xi(5))*(Yi(6)-Yi(1)) - (Yi(2)-Yi(5))*(Xi(6)-Xi(1))     
          ! Face)-5                                                            
          N(1,5)=(Yi(7)-Yi(2))*(Zi(6)-Zi(3)) - (Zi(7)-Zi(2))*(Yi(6)-Yi(3))     
          N(2,5)=(Zi(7)-Zi(2))*(Xi(6)-Xi(3)) - (Xi(7)-Xi(2))*(Zi(6)-Zi(3))     
          N(3,5)=(Xi(7)-Xi(2))*(Yi(6)-Yi(3)) - (Yi(7)-Yi(2))*(Xi(6)-Xi(3))     
          ! Face)-6                                                            
          N(1,6)=(Yi(8)-Yi(1))*(Zi(4)-Zi(5)) - (Zi(8)-Zi(1))*(Yi(4)-Yi(5))     
          N(2,6)=(Zi(8)-Zi(1))*(Xi(4)-Xi(5)) - (Xi(8)-Xi(1))*(Zi(4)-Zi(5))     
          N(3,6)=(Xi(8)-Xi(1))*(Yi(4)-Yi(5)) - (Yi(8)-Yi(1))*(Xi(4)-Xi(5))     
          !
          N(1:3,1) = HALF * N(1:3,1)
          N(1:3,2) = HALF * N(1:3,2)
          N(1:3,3) = HALF * N(1:3,3)
          N(1:3,4) = HALF * N(1:3,4)
          N(1:3,5) = HALF * N(1:3,5)
          N(1:3,6) = HALF * N(1:3,6)
          !
          VEL(1,1) = FOURTH*( V(1,NC(1)) + V(1,NC(2)) + V(1,NC(3)) + V(1,NC(4)) )
          VEL(1,2) = FOURTH*( V(2,NC(1)) + V(2,NC(2)) + V(2,NC(3)) + V(2,NC(4)) )
          VEL(1,3) = FOURTH*( V(3,NC(1)) + V(3,NC(2)) + V(3,NC(3)) + V(3,NC(4)) )
          !
          VEL(2,1) = FOURTH*( V(1,NC(3)) + V(1,NC(4)) + V(1,NC(7)) + V(1,NC(8)) )
          VEL(2,2) = FOURTH*( V(2,NC(3)) + V(2,NC(4)) + V(2,NC(7)) + V(2,NC(8)) )
          VEL(2,3) = FOURTH*( V(3,NC(3)) + V(3,NC(4)) + V(3,NC(7)) + V(3,NC(8)) )
          !
          VEL(3,1) = FOURTH*( V(1,NC(5)) + V(1,NC(6)) + V(1,NC(7)) + V(1,NC(8)) )
          VEL(3,2) = FOURTH*( V(2,NC(5)) + V(2,NC(6)) + V(2,NC(7)) + V(2,NC(8)) )
          VEL(3,3) = FOURTH*( V(3,NC(5)) + V(3,NC(6)) + V(3,NC(7)) + V(3,NC(8)) )
          !
          VEL(4,1) = FOURTH*( V(1,NC(1)) + V(1,NC(2)) + V(1,NC(5)) + V(1,NC(6)) )
          VEL(4,2) = FOURTH*( V(2,NC(1)) + V(2,NC(2)) + V(2,NC(5)) + V(2,NC(6)) )
          VEL(4,3) = FOURTH*( V(3,NC(1)) + V(3,NC(2)) + V(3,NC(5)) + V(3,NC(6)) )
          !
          VEL(5,1) = FOURTH*( V(1,NC(2)) + V(1,NC(3)) + V(1,NC(6)) + V(1,NC(7)) )
          VEL(5,2) = FOURTH*( V(2,NC(2)) + V(2,NC(3)) + V(2,NC(6)) + V(2,NC(7)) )
          VEL(5,3) = FOURTH*( V(3,NC(2)) + V(3,NC(3)) + V(3,NC(6)) + V(3,NC(7)) )
          !
          VEL(6,1) = FOURTH*( V(1,NC(1)) + V(1,NC(4)) + V(1,NC(5)) + V(1,NC(8)) )
          VEL(6,2) = FOURTH*( V(2,NC(1)) + V(2,NC(4)) + V(2,NC(5)) + V(2,NC(8)) )
          VEL(6,3) = FOURTH*( V(3,NC(1)) + V(3,NC(4)) + V(3,NC(5)) + V(3,NC(8)) )
          !
          DOTPROD(1,I) = VEL(1,1)*N(1,1) + VEL(1,2)*N(2,1) + VEL(1,3)*N(3,1)
          DOTPROD(2,I) = VEL(2,1)*N(1,2) + VEL(2,2)*N(2,2) + VEL(2,3)*N(3,2)
          DOTPROD(3,I) = VEL(3,1)*N(1,3) + VEL(3,2)*N(2,3) + VEL(3,3)*N(3,3)
          DOTPROD(4,I) = VEL(4,1)*N(1,4) + VEL(4,2)*N(2,4) + VEL(4,3)*N(3,4)
          DOTPROD(5,I) = VEL(5,1)*N(1,5) + VEL(5,2)*N(2,5) + VEL(5,3)*N(3,5)
          DOTPROD(6,I) = VEL(6,1)*N(1,6) + VEL(6,2)*N(2,6) + VEL(6,3)*N(3,6)
          !
          VOL(I) = ELBUF_TAB(NG)%GBUF%VOL(I)
        ENDDO 
      ELSEIF(N2D > 0 .AND. ITYP == 2)THEN !quads
        DO I=1,NEL
          IE=I+NFT    
          !---4 local node numbers NC1 TO NC4 for 2D solid element IE ---!                                                          
          NC(1)=IX(2,IE)                                                                                                            
          NC(2)=IX(3,IE)                                                                                                            
          NC(3)=IX(4,IE)                                                                                                            
          NC(4)=IX(5,IE)                                                                                                            
          !                                                                                                                         
          !---Coordinates of the 8 nodes                                                                                            
          Xi(1)=ZERO                                                                                                                
          Yi(1)=X(2,NC(1))                                                                                                          
          Zi(1)=X(3,NC(1))                                                                                                          
          !                                                                                                                         
          Xi(2)=ZERO                                                                                                                
          Yi(2)=X(2,NC(2))                                                                                                          
          Zi(2)=X(3,NC(2))                                                                                                          
          !                                                                                                                         
          Xi(3)=ZERO                                                                                                                
          Yi(3)=X(2,NC(3))                                                                                                          
          Zi(3)=X(3,NC(3))                                                                                                          
          !                                                                                                                         
          Xi(4)=ZERO                                                                                                                
          Yi(4)=X(2,NC(4))                                                                                                          
          Zi(4)=X(3,NC(4))                                                                                                          
          !                                                                                                                         
          ! Face)-1                                                                                                                 
          N(1,1) = ZERO 
          N(2,1) = (Zi(2)-Zi(1))                                                                           
          N(3,1) =-(Yi(2)-Yi(1))                                                                           
          ! Face)-2                                                                                        
          N(1,2) = ZERO                                                                                    
          N(2,2) = (Zi(3)-Zi(2))                                                                           
          N(3,2) =-(Yi(3)-Yi(2))                                                                           
          ! Face)-3                                                                                        
          N(1,3) = ZERO                                                                                    
          N(2,3) = (Zi(4)-Zi(3))                                                                           
          N(3,3) =-(Yi(4)-Yi(3))                                                                           
          ! Face)-4                                                                                        
          N(1,4) = ZERO                                                                                    
          N(2,4) = (Zi(1)-Zi(4))                                                                           
          N(3,4) =-(Yi(1)-Yi(4))  
          !
          N(1:3,5:6)=ZERO    
          !
          VEL(1,2)=HALF*(V(2,NC(1)) + V(2,NC(2)))
          VEL(1,3)=HALF*(V(3,NC(1)) + V(3,NC(2)))
          !
          VEL(2,2)=HALF*(V(2,NC(2)) + V(2,NC(3)))
          VEL(2,3)=HALF*(V(3,NC(2)) + V(3,NC(3)))
          !
          VEL(3,2)=HALF*(V(2,NC(3)) + V(2,NC(4)))
          VEL(3,3)=HALF*(V(3,NC(3)) + V(3,NC(4)))
          !
          VEL(4,2)=HALF*(V(2,NC(4)) + V(2,NC(1)))
          VEL(4,3)=HALF*(V(3,NC(4)) + V(3,NC(1)))
          !
          DOTPROD(1,I) = VEL(1,2)*N(2,1) + VEL(1,3)*N(3,1)
          DOTPROD(2,I) = VEL(2,2)*N(2,2) + VEL(2,3)*N(3,2)
          DOTPROD(3,I) = VEL(3,2)*N(2,3) + VEL(3,3)*N(3,3)
          DOTPROD(4,I) = VEL(4,2)*N(2,4) + VEL(4,3)*N(3,4)
          DOTPROD(5,I) = ZERO
          DOTPROD(6,I) = ZERO
          !
          IF(MLW /= 151)THEN
            A1 = Yi(2)*(Zi(3)-Zi(4))+Yi(3)*(Zi(4)-Zi(2))+Yi(4)*(Zi(2)-Zi(3))
            A2 = Yi(2)*(Zi(4)-Zi(1))+Yi(4)*(Zi(1)-Zi(2))+Yi(1)*(Zi(2)-Zi(4))
            AREA = (A1+A2)* HALF
          ELSE
           AREA = ELBUF_TAB(NG)%GBUF%AREA(I)
          ENDIF
          VOL(I) = AREA
        ENDDO                                                                                                                       
      ELSEIF(N2D > 0 .AND. ITYP == 7)THEN !triangles
        DO I=1,NEL
          IE=I+NFT                                                             
          !---3 local node numbers NC1 TO NC3 for 2D solid element IE ---!                                                          
          NC(1)=IX(2,IE)                                                                                                            
          NC(2)=IX(3,IE)                                                                                                            
          NC(3)=IX(4,IE) 
          !---Coordinates of the 8 nodes                                                                                            
          Xi(1)=ZERO                                                                                                                
          Yi(1)=X(2,NC(1))                                                                                                          
          Zi(1)=X(3,NC(1))                                                                                                          
          !                                                                                                                         
          Xi(2)=ZERO                                                                                                                
          Yi(2)=X(2,NC(2))                                                                                                          
          Zi(2)=X(3,NC(2))                                                                                                          
          !                                                                                                                         
          Xi(3)=ZERO                                                                                                                
          Yi(3)=X(2,NC(3))                                                                                                          
          Zi(3)=X(3,NC(3))                                                                                                          
          !      
          ! Face)-1                                                                                                                 
          N(1,1) = ZERO
          N(2,1) = (Zi(2)-Zi(1))                                                                              
          N(3,1) =-(Yi(2)-Yi(1))                                                                              
          ! Face)-2                                                                                           
          N(1,2) = ZERO                                                                                       
          N(2,2) = (Zi(3)-Zi(2))                                                                              
          N(3,2) =-(Yi(3)-Yi(2))                                                                              
          ! Face)-3                                                                                           
          N(1,3) = ZERO                                                                                       
          N(2,3) = (Zi(1)-Zi(3))                                                                              
          N(3,3) =-(Yi(1)-Yi(3)) 
          !
          N(1:3,4:6)=ZERO
          !
          VEL(1,2)=HALF*(V(2,NC(1)) + V(2,NC(2)))
          VEL(1,3)=HALF*(V(3,NC(1)) + V(3,NC(2)))
          !
          VEL(2,2)=HALF*(V(2,NC(2)) + V(2,NC(3)))
          VEL(2,3)=HALF*(V(3,NC(2)) + V(3,NC(3)))
          !
          VEL(3,2)=HALF*(V(2,NC(3)) + V(2,NC(4)))
          VEL(3,3)=HALF*(V(3,NC(3)) + V(3,NC(4)))
          !
          DOTPROD(1,I) = VEL(1,2)*N(2,1) + VEL(1,3)*N(3,1)
          DOTPROD(2,I) = VEL(2,2)*N(2,2) + VEL(2,3)*N(3,2)
          DOTPROD(3,I) = VEL(3,2)*N(2,3) + VEL(3,3)*N(3,3)
          DOTPROD(4,I) = ZERO
          DOTPROD(5,I) = ZERO
          DOTPROD(6,I) = ZERO
          !
          IF(MLW /= 151)THEN
            NX = HALF * ((Yi(2) - Yi(1)) * (Zi(3) - Zi(1)) - (Zi(2) - Zi(1)) * (Yi(3) - Yi(1)))
            AREA=ABS(NX)  !SQRT(NX*NX+NY*NY+NZ*NZ)
          ELSE
           AREA = ELBUF_TAB(NG)%GBUF%AREA(I)
          ENDIF
          VOL(I) = AREA
        ENDDO            
      ELSE
          VOL(1:NEL) = ONE
      ENDIF
        
      !-------------------------------------!
      !     FACE RECONSTRUCTION
      !-------------------------------------! 

        DO I=1,NEL
          IE=I+NFT 
          !-------------------------------------!
          !  DIVERGENCE (GREEN OSTROGRADSKI)
          !-------------------------------------! 
          DIV = ZERO
          DIV = DIV + DOTPROD(1,I)
          DIV = DIV + DOTPROD(2,I)
          DIV = DIV + DOTPROD(3,I)
          DIV = DIV + DOTPROD(4,I)
          DIV = DIV + DOTPROD(5,I)
          DIV = DIV + DOTPROD(6,I)
          DIV = DIV / VOL(I)
          EVAR(I) = DIV
          
        ENDDO!next I

      END SUBROUTINE OUTPUT_DIV_U
        
