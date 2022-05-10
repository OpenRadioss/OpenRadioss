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
Chd|  ALEW5                         source/ale/alew5.F            
Chd|-- called by -----------
Chd|        ALEWDX                        source/ale/alewdx.F           
Chd|-- calls ---------------
Chd|        ALEWDX_GRID_BCS               source/ale/alewdx_grid_bcs.F  
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        SPMD_XVOIS                    source/mpi/fluid/spmd_cfd.F   
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE ALEW5( 
     1     X         ,D       ,V       ,W       ,WA      ,
     2     ALE_NN_CONNECT     ,NALE    ,NODFT   ,NODLT   ,ITASK   ,
     3     NBRCVOIS  ,NBSDVOIS,LNRCVOIS,LNSDVOIS,ITAB    ,
     4     SKEW,ISKEW, ICODT)
C-----------------------------------------------
C     M o d u l e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C     D e s c r i p t i o n
C-----------------------------------------------
C     LAPLACIAN GRID SMOOTHING
C     Compute Grid for /ALE/GRID/LAPLACIAN 
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NALE(*), NODFT, NODLT, ITASK,
     .     NBRCVOIS(*),NBSDVOIS(*),
     .     LNRCVOIS(*),LNSDVOIS(*),ITAB(*),ISKEW(*),ICODT(*)
      my_real
     .     X(3,*), D(3,*), V(3,*), W(3,*), WA(3,*), SKEW(LSKEW,*)   
      TYPE(t_connectivity), INTENT(IN) :: ALE_NN_CONNECT
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com07_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "timeri_c.inc"      
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, IA(6), L, LENCOM, K, ITER,NITER,NUM
      my_real
     .     DX, DY, DZ,FAC, VEC(3),LAMBDA,ETA,IFORM, WEIGHT(6),SOM,SOM2,MIN_DIST,MAX_DIST
      my_real, DIMENSION(:), ALLOCATABLE :: DIST
      INTEGER :: IAD, IAD1, IAD2, NODE_ID
C-----------------------------------------------
C     D: is global displacement from t=0
C     W: is Grid displacement
C     WEIGHT : 1/n = umbrella
C     l_ij/sum(l_ik,k) = fujiwara operator   (we must update CFL with DT2< edge_min**2/LAMBDA)
C     
C     please note that W will be used as a displacement in this subroutine and translated into grid velocity again before returning 
C-----------------------------------------------
      LAMBDA=ALPHA
      NITER=NINT(VGX)
      IFORM=NINT(GAMMA)
      
      WA(1:3,NODFT:NODLT)=X(1:3,NODFT:NODLT)      
      CALL MY_BARRIER
      
      DO ITER=1,NITER

         IF(NSPMD.GT.1)THEN
!$OMP SINGLE
            LENCOM = NBRCVOIS(NSPMD+1)+NBSDVOIS(NSPMD+1)
            CALL SPMD_XVOIS(WA     ,NBRCVOIS,NBSDVOIS,LNRCVOIS,LNSDVOIS,LENCOM)
!$OMP END SINGLE
         END IF

         IF(IFORM.EQ.1)THEN     
         !default formulation (2020.0)         
            DO I = NODFT, NODLT
               IF(IABS(NALE(I)).EQ.1) THEN
                  IAD1 = ALE_NN_CONNECT%IAD_CONNECT(I)
                  IAD2 = ALE_NN_CONNECT%IAD_CONNECT(I + 1) - 1
                  NUM = 0
                  VEC(1:3) = ZERO
                  DO IAD = IAD1, IAD2
                     NUM = NUM + 1
                     NODE_ID = ALE_NN_CONNECT%CONNECTED(IAD)
                     VEC(1) = VEC(1) + WA(1, NODE_ID)
                     VEC(2) = VEC(2) + WA(2, NODE_ID)
                     VEC(3) = VEC(3) + WA(3, NODE_ID)
                  ENDDO
                  
                  VEC(1) = VEC(1) / NUM - WA(1,I)
                  VEC(2) = VEC(2) / NUM - WA(2,I)
                  VEC(3) = VEC(3) / NUM - WA(3,I)     

                  !update VEC depending on boundary conditions
                  CALL ALEWDX_GRID_BCS(SKEW, ISKEW,  ICODT, VEC, NALE, ITASK, I)   !set VEC to 0 depending on user BCS
                  W(1,I) = WA(1,I) + LAMBDA * VEC(1)
                  W(2,I) = WA(2,I) + LAMBDA * VEC(2)
                  W(3,I) = WA(3,I) + LAMBDA * VEC(3)
                                  
               ELSE
                  W(1,I) = WA(1,I)
                  W(2,I) = WA(2,I)
                  W(3,I) = WA(3,I)     
               ENDIF
            ENDDO
         ELSEIF(IFORM.EQ.2)THEN
         ! experimental formulation
            DO I = NODFT, NODLT
               IF(IABS(NALE(I)).EQ.1) THEN
                  IAD1 = ALE_NN_CONNECT%IAD_CONNECT(I)
                  IAD2 = ALE_NN_CONNECT%IAD_CONNECT(I + 1) - 1
                  NUM = 0
                  VEC(1:3) = ZERO
                  ALLOCATE(DIST(IAD2 - IAD1 + 1))                        
                  DO IAD = IAD1, IAD2
                     NUM = NUM + 1
                     NODE_ID = ALE_NN_CONNECT%CONNECTED(IAD)
                     VEC(1) = WA(1,NODE_ID) - WA(1,I)
                     VEC(2) = WA(2,NODE_ID) - WA(2,I)
                     VEC(3) = WA(3,NODE_ID) - WA(3,I)
                     DIST(IAD) = SQRT(VEC(1)*VEC(1)+VEC(2)*VEC(2)+VEC(3)*VEC(3))

                  ENDDO
                  SOM = SUM(DIST(1:NUM))
                  
                  MIN_DIST = MINVAL(DIST(1:NUM))
                  MAX_DIST = MAXVAL(DIST(1:NUM))             
                  SOM2 = ZERO
                  DO K=1,NUM
                     SOM2 = SOM2 + UN / DIST(K)
                  ENDDO
                  
                  DO K=1,NUM
                     WEIGHT(K)=DIST(K)/SOM
                  ENDDO          
                  VEC(1:3)=ZERO
                  DO IAD = IAD1, IAD2
                     NODE_ID = ALE_NN_CONNECT%CONNECTED(IAD)
                     VEC(1) = VEC(1) + WEIGHT(K) * (WA(1,NODE_ID) - WA(1,I))
                     VEC(2) = VEC(2) + WEIGHT(K) * (WA(2,NODE_ID) - WA(2,I))
                     VEC(3) = VEC(3) + WEIGHT(K) * (WA(3,NODE_ID) - WA(3,I))
                  ENDDO
                           
                  !update VEC depending on boundary conditions
                  CALL ALEWDX_GRID_BCS(SKEW, ISKEW,  ICODT, VEC ,NALE, ITASK, I)
                  W(1,I) = WA(1,I) + LAMBDA * VEC(1)
                  W(2,I) = WA(2,I) + LAMBDA * VEC(2)
                  W(3,I) = WA(3,I) + LAMBDA * VEC(3)                  

               ELSE
                  W(1,I)=WA(1,I)
                  W(2,I)=WA(2,I)
                  W(3,I)=WA(3,I)     
               ENDIF
               DEALLOCATE(DIST)
            ENDDO        
         ENDIF
         
         CALL MY_BARRIER
         WA(1:3,NODFT:NODLT)=W(1:3,NODFT:NODLT)
         
      ENDDO !next ITER
      

      DO I = NODFT, NODLT                              
         IF(IABS(NALE(I)).EQ.1 .AND. DT2.GT.ZERO) THEN  
            W(1,I)=(WA(1,I)-X(1,I))/DT2                             
            W(2,I)=(WA(2,I)-X(2,I))/DT2                             
            W(3,I)=(WA(3,I)-X(3,I))/DT2                             
         ELSEIF(NALE(I).EQ.0)THEN                       
            W(1,I)=V(1,I)                                 
            W(2,I)=V(2,I)                                 
            W(3,I)=V(3,I)                                 
         ELSE                                           
            W(1,I)=ZERO                                   
            W(2,I)=ZERO                                   
            W(3,I)=ZERO                                   
         ENDIF                                          
      ENDDO                                            

      RETURN
      END
