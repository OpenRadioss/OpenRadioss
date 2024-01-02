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
 
c TWO SOLUTIONS : 
C - mettre l'user ID dans ITHBUF(J)
C - rajouter CLUSTER(ILOC)%ID_GLOB dans le cluster (*)
Chd|====================================================================
Chd|  THCLUSTER                     source/output/th/thcluster.F  
Chd|-- called by -----------
Chd|        HIST2                         source/output/th/hist2.F      
Chd|-- calls ---------------
Chd|        SPMD_GLOB_DSUM9               source/mpi/interfaces/spmd_th.F
Chd|        WRTDES                        source/output/th/wrtdes.F     
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE THCLUSTER(WA   ,IAD   ,IADV   ,NN      ,NVAR     ,
     .                     ITTYP,ITHBUF,CLUSTER,SKEW    ,X        ,
     .                     IXS  ,IPARG )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD         
      USE CLUSTER_MOD
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IAD,IADV, NN, NVAR, ITTYP, ITHBUF(*)
        INTEGER IXS(NIXS,*),IPARG(NPARG,*)
        my_real WA(*),SKEW(LSKEW,*),X(3,*) 
        TYPE (CLUSTER_) ,DIMENSION(NCLUSTER) :: CLUSTER
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER :: I,J,L,II,JJ,KK,IVAR,ICLUSTER_L,IWA_L,NNOD,ISKN,
     .             N1,N2,N3,N4,NG,NFT
        my_real :: XM,YM,ZM,NORM,SX,SY,SZ,TX,TY,TZ,FS,FN,MB,MT
        my_real ,DIMENSION(3) :: VX,VY,VN,X1,FLOC,MLOC
C=======================================================================
      II = -1
      WA(1:NN*NVAR) = ZERO
      DO J=IAD,IAD+NN-1
        ICLUSTER_L = ITHBUF(J)
        II = II + 1
        IF (ICLUSTER_L > 0) THEN 
          IWA_L = 0
          DO L=IADV,IADV+NVAR-1
            IVAR = ITHBUF(L)
            IWA_L = IWA_L + 1
            IF (IVAR > 6 .and. IVAR < 11) THEN
              ISKN = CLUSTER(ICLUSTER_L)%SKEW
              NNOD = CLUSTER(ICLUSTER_L)%NNOD
              IF (ISKN > 0) THEN
                VX(1) = SKEW(1,ISKN)
                VX(2) = SKEW(2,ISKN)
                VX(3) = SKEW(3,ISKN)
                VY(1) = SKEW(4,ISKN)
                VY(2) = SKEW(5,ISKN)
                VY(3) = SKEW(6,ISKN)
                VN(1) = SKEW(7,ISKN)
                VN(2) = SKEW(8,ISKN)
                VN(3) = SKEW(9,ISKN)                      
c
              ELSE !  ISKN == 0 => calculate local skew
                X1(1:3) = ZERO
                DO JJ = 1,NNOD
                  N1 = CLUSTER(ICLUSTER_L)%NOD1(JJ)
                  X1(1) = X1(1) + X(1,N1)
                  X1(2) = X1(2) + X(2,N1)
                  X1(3) = X1(3) + X(3,N1)
                ENDDO
                XM = X1(1) / NNOD                            
                YM = X1(2) / NNOD                            
                ZM = X1(3) / NNOD
c
                VN(1) = ZERO                            
                VN(2) = ZERO                             
                VN(3) = ZERO                             
c
                IF (CLUSTER(ICLUSTER_L)%TYPE == 1) THEN      ! cohesive 3D elements    
                  DO KK = 1,CLUSTER(ICLUSTER_L)%NEL
                    NG  = CLUSTER(ICLUSTER_L)%NG(KK)                
                    JJ  = CLUSTER(ICLUSTER_L)%ELEM(KK)
                    NFT = IPARG(3,NG)
                    N1 = IXS(2,NFT+JJ)
                    N2 = IXS(3,NFT+JJ)
                    N3 = IXS(4,NFT+JJ)
                    N4 = IXS(5,NFT+JJ)
                    SX = X(1,N3) - X(1,N1)                 
                    SY = X(2,N3) - X(2,N1)                  
                    SZ = X(3,N3) - X(3,N1)
                    TX = X(1,N4) - X(1,N2)                  
                    TY = X(2,N4) - X(2,N2)                  
                    TZ = X(3,N4) - X(3,N2)
                    VN(1) = VN(1) + SY*TZ - SZ*TY                        
                    VN(2) = VN(2) + SZ*TX - SX*TZ                          
                    VN(3) = VN(3) + SX*TY - SY*TX                          
                  END DO
c
                ELSE   ! spring cluster
                  N1 = CLUSTER(ICLUSTER_L)%NOD1(NNOD)                                              
                  N2 = CLUSTER(ICLUSTER_L)%NOD1(1)                            
                  SX = XM - X(1,N1)               
                  SY = YM - X(2,N1)                
                  SZ = ZM - X(3,N1)
                  TX = XM - X(1,N2)                
                  TY = YM - X(2,N2)                
                  TZ = ZM - X(3,N2)
                  VN(1) = VN(1) + SY*TZ - SZ*TY                        
                  VN(2) = VN(2) + SZ*TX - SX*TZ                          
                  VN(3) = VN(3) + SX*TY - SY*TX                          
                  DO KK = 1,NNOD-1
                    N1 = CLUSTER(ICLUSTER_L)%NOD1(KK)                                              
                    N2 = CLUSTER(ICLUSTER_L)%NOD1(KK+1)                            
                    SX = XM - X(1,N1)               
                    SY = YM - X(2,N1)                
                    SZ = ZM - X(3,N1)
                    TX = XM - X(1,N2)                
                    TY = YM - X(2,N2)                
                    TZ = ZM - X(3,N2)
                    VN(1) = VN(1) + SY*TZ - SZ*TY                      
                    VN(2) = VN(2) + SZ*TX - SX*TZ                        
                    VN(3) = VN(3) + SX*TY - SY*TX                        
                  END DO
                END IF ! cluster type
c
                NORM = ONE / SQRT(VN(1)**2 + VN(2)**2 + VN(3)**2)        
                VN(1) = VN(1)*NORM                                               
                VN(2) = VN(2)*NORM                                               
                VN(3) = VN(3)*NORM                                               
c
c               calculate X and Y directions of the local skew
c
                N1 = CLUSTER(ICLUSTER_L)%NOD1(1)                                              
                N2 = CLUSTER(ICLUSTER_L)%NOD1(2)                            
                VX(1) = X(1,N1) - XM                         
                VX(2) = X(2,N1) - YM                         
                VX(3) = X(3,N1) - ZM 
                VY(1) = VN(2)*VX(3) - VN(3)*VX(2)                                  
                VY(2) = VN(3)*VX(1) - VN(1)*VX(3)                                  
                VY(3) = VN(1)*VX(2) - VN(2)*VX(1)                                  
                NORM = ONE / SQRT(VY(1)**2 + VY(2)**2 + VY(3)**2)        
                VY(1) = VY(1)*NORM                                               
                VY(2) = VY(2)*NORM                                               
                VY(3) = VY(3)*NORM                                               
                VX(1) = VY(2)*VN(3) - VY(3)*VN(2)                                
                VX(2) = VY(3)*VN(1) - VY(1)*VN(3)                                
                VX(3) = VY(1)*VN(2) - VY(2)*VN(1)                                
                NORM = ONE / SQRT(VX(1)**2 + VX(2)**2 + VX(3)**2)        
                VX(1) = VX(1)*NORM                                               
                VX(2) = VX(2)*NORM                                               
                VX(3) = VX(3)*NORM
              ENDIF   ! ISKN
c--------------------------------------------------------- 
              FLOC(1) = CLUSTER(ICLUSTER_L)%FOR(1)*VX(1) +  
     .                  CLUSTER(ICLUSTER_L)%FOR(2)*VX(2) +  
     .                  CLUSTER(ICLUSTER_L)%FOR(3)*VX(3)    
              FLOC(2) = CLUSTER(ICLUSTER_L)%FOR(1)*VY(1) +  
     .                  CLUSTER(ICLUSTER_L)%FOR(2)*VY(2) +  
     .                  CLUSTER(ICLUSTER_L)%FOR(3)*VY(3)    
              FLOC(3) = CLUSTER(ICLUSTER_L)%FOR(1)*VN(1) +  
     .                  CLUSTER(ICLUSTER_L)%FOR(2)*VN(2) +  
     .                  CLUSTER(ICLUSTER_L)%FOR(3)*VN(3)    
              MLOC(1) = CLUSTER(ICLUSTER_L)%MOM(1)*VX(1) +  
     .                  CLUSTER(ICLUSTER_L)%MOM(2)*VX(2) +  
     .                  CLUSTER(ICLUSTER_L)%MOM(3)*VX(3)    
              MLOC(2) = CLUSTER(ICLUSTER_L)%MOM(1)*VY(1) +  
     .                  CLUSTER(ICLUSTER_L)%MOM(2)*VY(2) +  
     .                  CLUSTER(ICLUSTER_L)%MOM(3)*VY(3)    
              MLOC(3) = CLUSTER(ICLUSTER_L)%MOM(1)*VN(1) +  
     .                  CLUSTER(ICLUSTER_L)%MOM(2)*VN(2) +  
     .                  CLUSTER(ICLUSTER_L)%MOM(3)*VN(3)    
            ENDIF  ! IVAR > 6 .and. IVAR < 11
c
            IF (IVAR==1) THEN                  ! Fx
              WA(IWA_L+(II)*NVAR) = CLUSTER(ICLUSTER_L)%FOR(1)   
            ELSEIF (IVAR==2) THEN              ! Fy
              WA(IWA_L+(II)*NVAR) = CLUSTER(ICLUSTER_L)%FOR(2)   
            ELSEIF (IVAR==3) THEN              ! Fz
              WA(IWA_L+(II)*NVAR) = CLUSTER(ICLUSTER_L)%FOR(3)   
            ELSEIF (IVAR==4) THEN              ! Mx
              WA(IWA_L+(II)*NVAR) = CLUSTER(ICLUSTER_L)%MOM(1) 
            ELSEIF (IVAR==5) THEN              ! My
              WA(IWA_L+(II)*NVAR) = CLUSTER(ICLUSTER_L)%MOM(2)   
            ELSEIF (IVAR==6) THEN              ! Mz
              WA(IWA_L+(II)*NVAR) = CLUSTER(ICLUSTER_L)%MOM(3)   
            ELSEIF (IVAR==7) THEN              ! F_shear
              FS = SQRT(FLOC(1)*FLOC(1) + FLOC(2)*FLOC(2))
              WA(IWA_L+(II)*NVAR) = FS
            ELSEIF (IVAR==8) THEN              ! F_normal
              FN = ABS(FLOC(3))
              WA(IWA_L+(II)*NVAR) = FN
            ELSEIF (IVAR==9) THEN              ! M_bend        
              MB = SQRT(MLOC(1)*MLOC(1) + MLOC(2)*MLOC(2))  
              WA(IWA_L+(II)*NVAR) = MB
            ELSEIF (IVAR==10) THEN             ! M_tors
              MT = ABS(MLOC(3))
              WA(IWA_L+(II)*NVAR) = MT
            ELSEIF (IVAR==11) THEN             ! DMG
              WA(IWA_L+(II)*NVAR) = CLUSTER(ICLUSTER_L)%FAIL              
            ENDIF
          END DO !L=IADV,IADV+NVAR-1
        ENDIF !IF I OWN THE JTH CLUSTER 
      END DO  !J=IAD,IAD+NN-1
c--------------------------       
      IF (NN*NVAR > 0) THEN 
        IF (NSPMD > 1) CALL SPMD_GLOB_DSUM9(WA,NN*NVAR)
        IF (ISPMD == 0) THEN 
           CALL WRTDES(WA,WA,NN*NVAR,ITTYP,1)
        ENDIF
      ENDIF
c-----------        
      RETURN
      END 
