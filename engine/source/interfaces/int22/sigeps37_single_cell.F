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
Chd|  SIGEPS37_SINGLE_CELL          source/interfaces/int22/sigeps37_single_cell.F
Chd|-- called by -----------
Chd|        SINIT22_FVM                   source/interfaces/int22/sinit22_fvm.F
Chd|-- calls ---------------
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        INITBUF_MOD                   share/resol/initbuf.F         
Chd|====================================================================
      SUBROUTINE SIGEPS37_SINGLE_CELL (
     1   ELBUF_TAB, IXS,  BUFMAT , IPARG, IPM,
     2   IDLOC    , NG ,  brickID, VOL  
     .                                )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C Interface Type22 (/INTER/TYPE22) is an FSI coupling method based on cut cell method. 
C   This experimental cut cell method is not completed, abandoned, and is not an official option.
C
C This subroutine is treating material law 37.
C    !UVAR(I,1) : massic percentage of liquid * global density  (rho1*V1/V : it needs to give liquid mass multiplying by element volume in aleconve.F)
C    !UVAR(I,2) : density of gas
C    !UVAR(I,3) : density of liquid
C    !UVAR(I,4) : volumetric fraction of liquid
C    !UVAR(I,5) : volumetric fraction of gas
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE INITBUF_MOD
      USE ELBUFDEF_MOD
      USE ALEFVM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   I n c l u d  e s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   I N P U T   A r g u m e n t s
C-----------------------------------------------
      my_real,INTENT(INOUT)      :: VOL
      INTEGER,INTENT(IN)         :: IDLOC,IPARG(NPARG,*), NG, IXS(NIXS,NUMELS), brickID, IPM(NPROPMI,*)
      TYPE(BUF_MAT_)  ,POINTER   :: MBUF
      my_real,INTENT(IN),TARGET  :: BUFMAT(*)
      TYPE(G_BUFEL_)  ,POINTER   :: GBUF
      TYPE(L_BUFEL_)  ,POINTER   :: LBUF
      TYPE(ELBUF_STRUCT_), TARGET, DIMENSION(NGROUP) :: ELBUF_TAB
C-----------------------------------------------
C   O U T P U T   A r g u m e n t s
C-----------------------------------------------

C-----------------------------------------------
C   I N P U T   O U T P U T   A r g u m e n t s 
C-----------------------------------------------

C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J, ISOLVER, NITER, ITER,MT, LLT_, IADBUF
      my_real 
     .   SSP,VIS,VIS2,VIS3,VV,C1,C2,C12,R1,R2,PMIN,A2,RHO10,RHO20,
     .   RHO1,RHO2,A1,A,B,C, B1,B2,P,GAM,P0,GPR,POLD,
     .   PN2,RHN2,VISA1,VISB1,VISA2,VISB2,DYDX,RHOSCALE,TOL,  MAS, MAS1, MAS2,
     .   RHO1T,RHO2T, ERROR, P1,P2,F1,F2,DF11,DF12,DF21,DF22,DET,DRHO1,DRHO2,
     .   MU1P1, MU2P1, DP0, PSH, SSP1, SSP2, RHO,  UVAR(5)
C-----------------------------------------------
C   S o u r c e   C o d e
C-----------------------------------------------

          !APPEL SIGEPS37 POUR EQUILIBRE PRESSION

          !
          LLT_                       =  IPARG(2,NG)
          MBUF                       => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1) 
          GBUF                       => ELBUF_TAB(NG)%GBUF
          LBUF                       => ELBUF_TAB(NG)%BUFLY(1)%LBUF(1,1,1)

          !print *, IDLOC,NG,LLT_

          UVAR(1) = MBUF%VAR((0*LLT_ + IDLOC))
          UVAR(2) = MBUF%VAR((1*LLT_ + IDLOC))
          UVAR(3) = MBUF%VAR((2*LLT_ + IDLOC))
          UVAR(4) = MBUF%VAR((3*LLT_ + IDLOC))
          UVAR(5) = MBUF%VAR((4*LLT_ + IDLOC))
          
           
          MT                         =  IXS(1,brickID)
          IADBUF                     =  IPM(7,MT) 
          RHO10                      =  BUFMAT(IADBUF-1+11)
          RHO20                      =  BUFMAT(IADBUF-1+12)
          R1                         =  BUFMAT(IADBUF-1+06)
          P0                         =  BUFMAT(IADBUF-1+09)
          GAM                        =  BUFMAT(IADBUF-1+05)
          C1                         =  BUFMAT(IADBUF-1+04)
          PMIN                       =  BUFMAT(IADBUF-1+08)
          PSH                        =  BUFMAT(IADBUF-1+16)
          
          RHO                        = GBUF%RHO(IDLOC)
          
          UVAR(1) =  MBUF%VAR((1-1)*LLT_ +IDLOC)
          UVAR(2) =  MBUF%VAR((2-1)*LLT_ +IDLOC)
          UVAR(3) =  MBUF%VAR((3-1)*LLT_ +IDLOC)
          UVAR(4) =  MBUF%VAR((4-1)*LLT_ +IDLOC)
          UVAR(5) =  MBUF%VAR((5-1)*LLT_ +IDLOC)
          
           !  if(ixs(11,brickID)==1804589)then
           !    print * ,"link switch"
           !    print *, " ini_iter :rho1.V1/V UVAR(1) =", IXS(11,brickID), UVAR(1)
           !    print *, " ini_iter :rho2      UVAR(2) =", IXS(11,brickID), UVAR(2)
           !    print *, " ini_iter :rho1      UVAR(3) =", IXS(11,brickID), UVAR(3)
           !    print *, " ini_iter :alp1      UVAR(4) =", IXS(11,brickID), UVAR(4)
           !    print *, " ini_iter :alp2      UVAR(5) =", IXS(11,brickID), UVAR(5)  
           !    print *, " ini_iter :v1                =", IXS(11,brickID), UVAR(4)*VOL  
           !    print *, " ini_iter :v2                =", IXS(11,brickID), UVAR(5)*VOL
           !    print *, " ini_iter :v=v1+v2           =", IXS(11,brickID), VOL
           !  endif          
           !------------------------------------!                                         
           !    EQUILIBRIUM ITERATIONS (T>0)    !                                         
           !------------------------------------!                                         
           TOL = EM10                                                                     
           NITER = 20                                                                     
                                                                               
             MAS = RHO * VOL                                                              
             MAS1 = UVAR(1) * VOL                                                         
             MAS2 = MAS - MAS1                                                            
             RHO2 = UVAR( 2)                                                              
             RHO1 = UVAR( 3)                                                              
             IF (MAS1 / MAS < EM10) THEN                                               
                !! Phase 2 seule presente                                                 
                UVAR( 1) = ZERO                                                           
                UVAR( 4) = ZERO                                                           
                UVAR( 5) = ONE                                                             
                RHO2 = MAS / VOL                                                          
                UVAR( 2) = RHO2                                                           
                P = P0 * (RHO2/RHO20)**GAM                                                
             ELSEIF (MAS2 / MAS < EM10) THEN                                           
                !! Phase 1 seule presente                                                 
                RHO1 = MAS / VOL                                                          
                UVAR( 1) = RHO1                                                           
                UVAR( 3) = RHO1                                                           
                UVAR( 4) = ONE                                                             
                UVAR( 5) = ZERO                                                           
                P = R1 * RHO1 - C1 + P0                                                   
             ELSE                                                                         
                ERROR = EP30                                                              
                ITER  = 1                                                                 
                DO WHILE(ITER < NITER .AND. ERROR > TOL)                            
                   P1 = R1 * RHO1 - C1 + P0                                               
                   P2 = P0 * (RHO2/RHO20)**GAM                                            
                   F1 = MAS1 / RHO1 + MAS2 / RHO2 - VOL                                   
                   F2 = P1 - P2                                                           
                   DF11 = - MAS1 / (RHO1 * RHO1)                                          
                   DF12 = - MAS2 / (RHO2 * RHO2)                                          
                   DF21 = R1                                                              
                   DF22 = - GAM * P0 / (RHO20**GAM) * RHO2**(GAM - ONE)                    
                   DET = DF11 * DF22 - DF12 * DF21                                        
                   DRHO1 = (-DF22 * F1 + DF12 * F2) / DET                                 
                   DRHO2 = (DF21 * F1 - DF11 * F2) / DET                                  
                   DRHO1 = MIN(THREE * RHO1, MAX(DRHO1, - HALF * RHO1))                 
                   DRHO2 = MIN(THREE * RHO2, MAX(DRHO2, - HALF * RHO2))                 
                   RHO1 = RHO1 + DRHO1                                                    
                   RHO2 = RHO2 + DRHO2                                                    
                   ERROR = ABS(DRHO1 / RHO1) + ABS(DRHO2 / RHO2)                          
                   ITER = ITER + 1                                                        
                ENDDO                                                                     
                IF (ABS(P1 - P2) > 1.D-5) THEN                                         
                   !PRINT*, "P1", P1, "P2", P2                                            
                ENDIF                                                                     
                IF (ERROR > TOL) THEN                                                  
                   PRINT*, "*** WARNING LAW37, convergence tol. ", ERROR, TOL        
                ENDIF                                                                     
                P = R1 * RHO1 - C1 + P0                                                   
             ENDIF                                                                        
             SSP1 = R1 * RHO1                                                             
             SSP2 = GAM * P0 * (RHO2/RHO20)**GAM                                          
             ! P2 = P0 * ((RHO2/RHO20)**GAM-ONE)                                           
             !---output                                                                   
             UVAR(2)  = RHO2                                                              
             UVAR(3)  = RHO1                                                              
             UVAR(4)  = UVAR(1)/RHO1                                                      
             IF(UVAR(4)<EM20)UVAR(4)=ZERO                                              
             UVAR(5)  = ONE-UVAR(4)                                                        
             IF (SSP1 > ZERO) THEN                                                     
                SSP1 = UVAR(4) / SSP1                                                     
             ELSE                                                                         
                SSP1 = ZERO                                                               
             ENDIF                                                                        
             IF (SSP2 > ZERO) THEN                                                     
                SSP2 = UVAR(5) / SSP2                                                     
             ELSE                                                                         
                SSP2 = ZERO                                                               
             ENDIF                                                                        
             SSP = SSP1 + SSP2                                                            
             SSP = SQRT(ONE / SSP / RHO)                                                   
             !print *, "SSP :", SSP, SQRT(R1), SQRT(GAM*P/RHO2)                           
             P          = MAX(PMIN, P) + PSH                                              
             !SOUNDSP(I) = SSP                                                            
             UVAR(1)  = MAX(ZERO, UVAR(1))                                                
             UVAR(2)  = MAX(ZERO, UVAR(2))                                                
             UVAR(3)  = MAX(ZERO, UVAR(3))                                                
             UVAR(4)  = MAX(ZERO, UVAR(4))                                                
             UVAR(5)  = MAX(ZERO, UVAR(5))  
             
                                                           
             !STORE IN BUFFER                                                             
              MBUF%VAR((1-1)*LLT_ +IDLOC) = UVAR(1)                                        
              MBUF%VAR((2-1)*LLT_ +IDLOC) = UVAR(2)                                        
              MBUF%VAR((3-1)*LLT_ +IDLOC) = UVAR(3)                                        
              MBUF%VAR((4-1)*LLT_ +IDLOC) = UVAR(4)                                        
              MBUF%VAR((5-1)*LLT_ +IDLOC) = UVAR(5)                                        
                                                                                          
             ! if(ixs(11,brickID)==1804589)then                                           
                !print *, " end_iter :rho1.V1/V UVAR(1) =", IXS(11,brickID), UVAR(1)       
                !print *, " end_iter :rho2 UVAR(2)      =", IXS(11,brickID), UVAR(2)       
                !print *, " end_iter :rho1 UVAR(3)      =", IXS(11,brickID), UVAR(3)       
                !print *, " end_iter :alp1 UVAR(4)      =", IXS(11,brickID), UVAR(4)       
                !print *, " end_iter :alp2 UVAR(5)      =", IXS(11,brickID), UVAR(5)       
                !print *, " end_iter :v1                =", IXS(11,brickID), UVAR(4)*VOL   
                !print *, " end_iter :v2                =", IXS(11,brickID), UVAR(5)*VOL   
                !print *, " end_iter :v=v1+v2           =", IXS(11,brickID), VOL           
                !print *, " end_iter :c                 =", IXS(11,brickID), SSP           
                !print *, " end_iter :P                 =", IXS(11,brickID), P             
            !  endif                                                                      
         
           LBUF%SSP(IDLOC)            = SSP    ;                                    



           FCELL(5, brickID ) = SSP                                         
           FCELL(6, brickID ) = P  

C-----------------------------------------------
      RETURN
      END




