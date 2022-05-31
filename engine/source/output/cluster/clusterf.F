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
Chd|  CLUSTERF                      source/output/cluster/clusterf.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE CLUSTERF(CLUSTER  ,ELBUF_TAB,X       ,A       ,AR       ,
     .                    SKEW     ,IXS      ,IPARG   ,FCLUSTER,MCLUSTER ,
     .                    H3D_DATA ,GEO      ) 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD            
      USE CLUSTER_MOD         
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "param_c.inc"
#include "units_c.inc"
#include "comlock.inc"
#include "com01_c.inc"
#include "com04_c.inc"
#include "com08_c.inc"
#include "scr14_c.inc"
#include "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,*),IPARG(NPARG,*)
      my_real ,DIMENSION(3,*) :: X,A,AR,FCLUSTER,MCLUSTER      
      my_real ,DIMENSION(LSKEW,*) :: SKEW     
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER)   :: CLUSTER
      TYPE (ELBUF_STRUCT_),DIMENSION(NGROUP) :: ELBUF_TAB
      TYPE (H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,IL,IEL,NG,NFT,NNOD,ISKN,N,N1,N2,N3,N4,NINDX,IFAIL,IPID
      INTEGER CLUSTERNOD(NCLUSTER),LCLUSTER(NCLUSTER),LCL(NCLUSTER),
     .        IAD(NCLUSTER)
      INTEGER INDX(NCLUSTER)
      my_real, DIMENSION(NPROPG,*) :: GEO
      my_real, DIMENSION(3) :: FBOT,FTOP,MBOT,MTOP,M1,XG,X1,X2
      my_real, DIMENSION(3,NCLUSTER) :: VN,VX,VY
      my_real :: FN,FT,MR,MB,DMG,XM,YM,ZM,DX1,DY1,DZ1,DX2,DY2,DZ2,
     .      FX,FY,FZ,MOMX,MOMY,MOMZ,NORM,CRITF,CRITM,DRX,DRY,DRZ,
     .      SX,SY,SZ,TX,TY,TZ
      my_real, DIMENSION(NCLUSTER) :: TTHICK
C=======================================================================       
      NINDX = 0
      TTHICK(1:NCLUSTER) = ZERO
c
      DO I = 1, NCLUSTER
        IF (CLUSTER(I)%OFF == 0) CYCLE
        NNOD = CLUSTER(I)%NNOD                                             
        ISKN = CLUSTER(I)%SKEW                                             
        IFAIL= CLUSTER(I)%IFAIL
c------
c       center of moments on lower face
c------
        X1(1:3) = ZERO                                                          
        DO J = 1,NNOD                                                        
          N1 = CLUSTER(I)%NOD1(J)                                            
          X1(1) = X1(1) + X(1,N1)                                            
          X1(2) = X1(2) + X(2,N1)                                            
          X1(3) = X1(3) + X(3,N1)  
        END DO                                                               
        XM = X1(1) / NNOD                            
        YM = X1(2) / NNOD                            
        ZM = X1(3) / NNOD
c------   
c       local skew
c------
        IF (IFAIL > 0 .and. ISKN == 0) THEN   ! local skew fixed on bottom surf
c
c         calculate normal direction of the local skew
          VN(1,I) = ZERO                            
          VN(2,I) = ZERO                             
          VN(3,I) = ZERO                             
c
          IF (CLUSTER(I)%TYPE == 1) THEN      ! cohesive 3D elements    
            DO J = 1,CLUSTER(I)%NEL
              NG  = CLUSTER(I)%NG(J)                
              IEL = CLUSTER(I)%ELEM(J)
              NFT = IPARG(3,NG)
              IPID = IXS(10,NFT+IEL)
              N1 = IXS(2,NFT+IEL)
              N2 = IXS(3,NFT+IEL)
              N3 = IXS(4,NFT+IEL)
              N4 = IXS(5,NFT+IEL)
              TTHICK(I) = GEO(41,IPID)
              SX = X(1,N3) - X(1,N1)                 
              SY = X(2,N3) - X(2,N1)                  
              SZ = X(3,N3) - X(3,N1)
              TX = X(1,N4) - X(1,N2)                  
              TY = X(2,N4) - X(2,N2)                  
              TZ = X(3,N4) - X(3,N2)
              VN(1,I) = VN(1,I) + SY*TZ - SZ*TY                        
              VN(2,I) = VN(2,I) + SZ*TX - SX*TZ                          
              VN(3,I) = VN(3,I) + SX*TY - SY*TX                          
            END DO
c
          ELSE   ! spring cluster
            N1 = CLUSTER(I)%NOD1(NNOD)                                              
            N2 = CLUSTER(I)%NOD1(1)                            
            SX = XM - X(1,N1)               
            SY = YM - X(2,N1)                
            SZ = ZM - X(3,N1)
            TX = XM - X(1,N2)                
            TY = YM - X(2,N2)                
            TZ = ZM - X(3,N2)
            VN(1,I) = VN(1,I) + SY*TZ - SZ*TY                        
            VN(2,I) = VN(2,I) + SZ*TX - SX*TZ                          
            VN(3,I) = VN(3,I) + SX*TY - SY*TX                          
            DO J = 1,NNOD-1
              N1 = CLUSTER(I)%NOD1(J)                                              
              N2 = CLUSTER(I)%NOD1(J+1)                            
              SX = XM - X(1,N1)               
              SY = YM - X(2,N1)                
              SZ = ZM - X(3,N1)
              TX = XM - X(1,N2)                
              TY = YM - X(2,N2)                
              TZ = ZM - X(3,N2)
              VN(1,I) = VN(1,I) + SY*TZ - SZ*TY                      
              VN(2,I) = VN(2,I) + SZ*TX - SX*TZ                        
              VN(3,I) = VN(3,I) + SX*TY - SY*TX                        
            END DO
          END IF ! cluster type
c
          NORM = ONE / SQRT(VN(1,I)**2 + VN(2,I)**2 + VN(3,I)**2)        
          VN(1,I) = VN(1,I)*NORM                                               
          VN(2,I) = VN(2,I)*NORM                                               
          VN(3,I) = VN(3,I)*NORM                                               
c
c         calculate X and Y directions of the local skew
c
          N1 = CLUSTER(I)%NOD1(1)                                              
          N2 = CLUSTER(I)%NOD1(2)                            
          VX(1,I) = X(1,N1) - XM                         
          VX(2,I) = X(2,N1) - YM                         
          VX(3,I) = X(3,N1) - ZM 
          VY(1,I) = VN(2,I)*VX(3,I) - VN(3,I)*VX(2,I)                                  
          VY(2,I) = VN(3,I)*VX(1,I) - VN(1,I)*VX(3,I)                                  
          VY(3,I) = VN(1,I)*VX(2,I) - VN(2,I)*VX(1,I)                                  
          NORM = ONE / SQRT(VY(1,I)**2 + VY(2,I)**2 + VY(3,I)**2)        
          VY(1,I) = VY(1,I)*NORM                                               
          VY(2,I) = VY(2,I)*NORM                                               
          VY(3,I) = VY(3,I)*NORM                                               
          VX(1,I) = VY(2,I)*VN(3,I) - VY(3,I)*VN(2,I)                                
          VX(2,I) = VY(3,I)*VN(1,I) - VY(1,I)*VN(3,I)                                
          VX(3,I) = VY(1,I)*VN(2,I) - VY(2,I)*VN(1,I)                                
          NORM = ONE / SQRT(VX(1,I)**2 + VX(2,I)**2 + VX(3,I)**2)        
          VX(1,I) = VX(1,I)*NORM                                               
          VX(2,I) = VX(2,I)*NORM                                               
          VX(3,I) = VX(3,I)*NORM
c
        ENDIF    !  IFAIL > 0 .and. ISKN == 0
c------
c       Forces
c------
        FBOT = ZERO                                                        
        FTOP = ZERO                                                        
        DO J = 1,NNOD                                                    
          N1 = CLUSTER(I)%NOD1(J)                                            
          N2 = CLUSTER(I)%NOD2(J)
          FBOT(1) = FBOT(1) + A(1,N1)                                      
          FBOT(2) = FBOT(2) + A(2,N1)                                      
          FBOT(3) = FBOT(3) + A(3,N1)                                      
          FTOP(1) = FTOP(1) + A(1,N2)                                      
          FTOP(2) = FTOP(2) + A(2,N2)                                      
          FTOP(3) = FTOP(3) + A(3,N2)                                      
        END DO                                                           
c------
c       Moments
c------
        MBOT = ZERO                                                      
        MTOP = ZERO                                                      
c
        IF (CLUSTER(I)%TYPE == 1 .and. ISKN == 0 .and. TTHICK(I) > ZERO) THEN
          DO J = 1,NNOD                                                    
            N1  = CLUSTER(I)%NOD1(J)                                       
            N2  = CLUSTER(I)%NOD2(J)                                       
                                                                   
            DRX = X(1,N2) - XM                                     
            DRY = X(2,N2) - YM                                     
            DRZ = SIGN(TTHICK(I), X(3,N2) - ZM) 
            MTOP(1) = MTOP(1) + DRY*A(3,N2) - DRZ*A(2,N2)          
            MTOP(2) = MTOP(2) + DRZ*A(1,N2) - DRX*A(3,N2)          
            MTOP(3) = MTOP(3) + DRX*A(2,N2) - DRY*A(1,N2)          
c
            DRX = X(1,N1) - XM                                     
            DRY = X(2,N1) - YM                                     
            MBOT(1) = MBOT(1) + DRY*A(3,N1)         
            MBOT(2) = MBOT(2) - DRX*A(3,N1)            
            MBOT(3) = MBOT(3) + DRX*A(2,N1) - DRY*A(1,N1)            
          END DO  ! NNOD
        ELSE
          DO J = 1,NNOD                                                    
            N1  = CLUSTER(I)%NOD1(J)                                       
            N2  = CLUSTER(I)%NOD2(J)                                       
                                                                   
            DRX = X(1,N2) - XM                                     
            DRY = X(2,N2) - YM                                     
            DRZ = X(3,N2) - ZM                                     
            MTOP(1) = MTOP(1) + DRY*A(3,N2) - DRZ*A(2,N2)          
            MTOP(2) = MTOP(2) + DRZ*A(1,N2) - DRX*A(3,N2)          
            MTOP(3) = MTOP(3) + DRX*A(2,N2) - DRY*A(1,N2)          
c
            DRX = X(1,N1) - XM                                     
            DRY = X(2,N1) - YM                                     
            DRZ = X(3,N1) - ZM                                     
            MBOT(1) = MBOT(1) + DRY*A(3,N1) - DRZ*A(2,N1)            
            MBOT(2) = MBOT(2) + DRZ*A(1,N1) - DRX*A(3,N1)            
            MBOT(3) = MBOT(3) + DRX*A(2,N1) - DRY*A(1,N1)            
          END DO  ! NNOD
        END IF                                    
c------
        IF (CLUSTER(I)%TYPE == 1) THEN    ! Brick cluster                 
          FX   = (FTOP(1) - FBOT(1))*HALF          
          FY   = (FTOP(2) - FBOT(2))*HALF          
          FZ   = (FTOP(3) - FBOT(3))*HALF          
          MOMX = (MTOP(1) - MBOT(1))*HALF                           
          MOMY = (MTOP(2) - MBOT(2))*HALF                            
          MOMZ = (MTOP(3) - MBOT(3))*HALF                            
        ELSE    ! Spring cluster                 
          FX   = FTOP(1)        
          FY   = FTOP(2)        
          FZ   = FTOP(3)        
          MOMX = MTOP(1)                          
          MOMY = MTOP(2)                           
          MOMZ = MTOP(3)                           
          DO J = 1,NNOD                                                    
            N1  = CLUSTER(I)%NOD1(J)                                       
            N2  = CLUSTER(I)%NOD2(J)                                       
            MOMX = MOMX + AR(1,N2)                                         
            MOMY = MOMY + AR(2,N2)                                         
            MOMZ = MOMZ + AR(3,N2)
          END DO                                                           
        ENDIF              
c
        CLUSTER(I)%FOR(1) = FX         
        CLUSTER(I)%FOR(2) = FY         
        CLUSTER(I)%FOR(3) = FZ         
        CLUSTER(I)%MOM(1) = MOMX                          
        CLUSTER(I)%MOM(2) = MOMY                          
        CLUSTER(I)%MOM(3) = MOMZ                          
c------
      ENDDO  !  I = 1, NCLUSTER
c
c------------------------------      
c     Cluster  failure
c---------------------------------
      NINDX = 0
      INDX  = 0
c
      DO I = 1, NCLUSTER

        IF (CLUSTER(I)%OFF == 0) THEN
          CLUSTER(I)%FOR(1) = ZERO   
          CLUSTER(I)%FOR(2) = ZERO        
          CLUSTER(I)%FOR(3) = ZERO                 
          CLUSTER(I)%MOM(1) = ZERO                
          CLUSTER(I)%MOM(2) = ZERO                
          CLUSTER(I)%MOM(3) = ZERO                
        END IF
                                      
        NNOD = CLUSTER(I)%NNOD                                             
        ISKN = CLUSTER(I)%SKEW                                             
        IFAIL= CLUSTER(I)%IFAIL

c---        
c        IF (CLUSTER(I)%TYPE == 1) THEN  ! check local failure
cc        IF (CLUSTER(I)%IFAIL >= 10) THEN
cc         check local element failure : /FAIL
cc          IFAIL = IFAIl - 10
cc
c          NOFF = 0
c          DO J = 1,CLUSTER(I)%NEL
c            NG  = CLUSTER(I)%NG(J)
c            IEL = CLUSTER(I)%ELEM(J)
c            IF (ELBUF_TAB(NG)%GBUF%OFF(IEL) == ZERO) NOFF = NOFF + 1
c          END DO
c          IF (NOFF == CLUSTER(I)%NEL) THEN
c            CLUSTER(I)%OFF = 0
c            NINDX = NINDX+1  
c            INDX(NINDX) = I  
c            IDEL7NOK    = 1  
c          ENDIF
c        ENDIF
c        ENDIF
c---        
        IF (IFAIL > 0) THEN
          IF (ISKN > 0) THEN
            FBOT(1) = CLUSTER(I)%FOR(1)*SKEW(1,ISKN) +
     .              CLUSTER(I)%FOR(2)*SKEW(2,ISKN) +
     .              CLUSTER(I)%FOR(3)*SKEW(3,ISKN) 
            FBOT(2) = CLUSTER(I)%FOR(1)*SKEW(4,ISKN) +
     .              CLUSTER(I)%FOR(2)*SKEW(5,ISKN) +
     .              CLUSTER(I)%FOR(3)*SKEW(6,ISKN) 
            FBOT(3) = CLUSTER(I)%FOR(1)*SKEW(7,ISKN) +
     .              CLUSTER(I)%FOR(2)*SKEW(8,ISKN) +
     .              CLUSTER(I)%FOR(3)*SKEW(9,ISKN) 
            M1(1) = CLUSTER(I)%MOM(1)*SKEW(1,ISKN) +
     .              CLUSTER(I)%MOM(2)*SKEW(2,ISKN) +
     .              CLUSTER(I)%MOM(3)*SKEW(3,ISKN) 
            M1(2) = CLUSTER(I)%MOM(1)*SKEW(4,ISKN) +
     .              CLUSTER(I)%MOM(2)*SKEW(5,ISKN) +
     .              CLUSTER(I)%MOM(3)*SKEW(6,ISKN) 
            M1(3) = CLUSTER(I)%MOM(1)*SKEW(7,ISKN) +
     .              CLUSTER(I)%MOM(2)*SKEW(8,ISKN) +
     .              CLUSTER(I)%MOM(3)*SKEW(9,ISKN) 
          ELSE
            FBOT(1) = CLUSTER(I)%FOR(1)*VX(1,I) +
     .                CLUSTER(I)%FOR(2)*VX(2,I) +
     .                CLUSTER(I)%FOR(3)*VX(3,I) 
            FBOT(2) = CLUSTER(I)%FOR(1)*VY(1,I) +
     .                CLUSTER(I)%FOR(2)*VY(2,I) +
     .                CLUSTER(I)%FOR(3)*VY(3,I) 
            FBOT(3) = CLUSTER(I)%FOR(1)*VN(1,I) +
     .                CLUSTER(I)%FOR(2)*VN(2,I) +
     .                CLUSTER(I)%FOR(3)*VN(3,I) 
            M1(1) =   CLUSTER(I)%MOM(1)*VX(1,I) +
     .                CLUSTER(I)%MOM(2)*VX(2,I) +
     .                CLUSTER(I)%MOM(3)*VX(3,I) 
            M1(2) =   CLUSTER(I)%MOM(1)*VY(1,I) +
     .                CLUSTER(I)%MOM(2)*VY(2,I) +
     .                CLUSTER(I)%MOM(3)*VY(3,I) 
            M1(3) =   CLUSTER(I)%MOM(1)*VN(1,I) +
     .                CLUSTER(I)%MOM(2)*VN(2,I) +
     .                CLUSTER(I)%MOM(3)*VN(3,I) 
 

          ENDIF !  IF (ISKN > 0) THEN
          FN = ABS(FBOT(3))
          FT = SQRT(FBOT(1)*FBOT(1) + FBOT(2)*FBOT(2))
          MR = ABS(M1(3))
          MB = SQRT(M1(1)*M1(1) + M1(2)*M1(2))
        ENDIF !      IF (IFAIL > 0) THEN

c---------------------------
        IF (IFAIL == 1) THEN
c         Monodirectional + one direction 
          CRITF = MAX(FN/CLUSTER(I)%FMAX(1),FT/CLUSTER(I)%FMAX(2))
          CRITM = MAX(MR/CLUSTER(I)%MMAX(1),MB/CLUSTER(I)%MMAX(2))
          DMG   = MAX(CRITF,CRITM)

        ELSEIF (IFAIL == 2) THEN
c         Monodirectional + all directions
          DMG   = FOURTH*(MIN(ONE+EM10, FN/CLUSTER(I)%FMAX(1)) + 
     .                   MIN(ONE+EM10, FT/CLUSTER(I)%FMAX(2)) +
     .                   MIN(ONE+EM10, MR/CLUSTER(I)%MMAX(1)) + 
     .                   MIN(ONE+EM10, MB/CLUSTER(I)%MMAX(2)))

        ELSEIF (IFAIL == 3) THEN
c         Multidirectional failure
          DMG = 
     .        CLUSTER(I)%AX(1)*(FN/CLUSTER(I)%FMAX(1))**CLUSTER(I)%NX(1)
     .      + CLUSTER(I)%AX(2)*(FT/CLUSTER(I)%FMAX(2))**CLUSTER(I)%NX(2)
     .      + CLUSTER(I)%AX(3)*(MR/CLUSTER(I)%MMAX(1))**CLUSTER(I)%NX(3)
     .      + CLUSTER(I)%AX(4)*(MB/CLUSTER(I)%MMAX(2))**CLUSTER(I)%NX(4)
        ELSE  ! no fail 
          DMG = ZERO
        ENDIF
        CLUSTER(I)%FAIL = DMG
c---------------------------
        IF (DMG > ONE) THEN

          NINDX = NINDX+1  
          INDX(NINDX) = I                          
          IDEL7NOK    = 1                      
          CLUSTER(I)%OFF = 0  
          CLUSTER(I)%FOR(1) = ZERO   
          CLUSTER(I)%FOR(2) = ZERO        
          CLUSTER(I)%FOR(3) = ZERO                 
          CLUSTER(I)%MOM(1) = ZERO                
          CLUSTER(I)%MOM(2) = ZERO                
          CLUSTER(I)%MOM(3) = ZERO                
          IF (CLUSTER(I)%TYPE == 1) THEN           
            DO J = 1,CLUSTER(I)%NEL                
              NG  = CLUSTER(I)%NG(J)                
              IEL = CLUSTER(I)%ELEM(J) 
              ELBUF_TAB(NG)%GBUF%OFF(IEL) = ZERO
            END DO
          ELSE       ! spring cluster      
            !  set OFF flag to zero for all elements                        
          ENDIF                                    
        ENDIF   
c
      ENDDO   !  I = 1, NCLUSTER
c---------------------------
      IF (ANIM_V(19) + H3D_DATA%N_VECT_CLUST_FORCE > 0) THEN
        DO I = 1, NCLUSTER
          NNOD = CLUSTER(I)%NNOD
          DO J = 1,NNOD
            N = CLUSTER(I)%NOD1(J)
            FCLUSTER(1,N) = CLUSTER(I)%FOR(1)
            FCLUSTER(2,N) = CLUSTER(I)%FOR(2)
            FCLUSTER(3,N) = CLUSTER(I)%FOR(3)
            N = CLUSTER(I)%NOD2(J)
            FCLUSTER(1,N) = CLUSTER(I)%FOR(1)
            FCLUSTER(2,N) = CLUSTER(I)%FOR(2)
            FCLUSTER(3,N) = CLUSTER(I)%FOR(3)
          ENDDO
        ENDDO  !  I = 1, NCLUSTER
      ENDIF     
      IF (ANIM_V(20) + H3D_DATA%N_VECT_CLUST_MOM > 0) THEN
        DO I = 1, NCLUSTER
          NNOD = CLUSTER(I)%NNOD
          DO J = 1,NNOD
            N = CLUSTER(I)%NOD1(J)
            MCLUSTER(1,N) = CLUSTER(I)%MOM(1)
            MCLUSTER(2,N) = CLUSTER(I)%MOM(2)
            MCLUSTER(3,N) = CLUSTER(I)%MOM(3)
            N = CLUSTER(I)%NOD2(J)
            MCLUSTER(1,N) = CLUSTER(I)%MOM(1)
            MCLUSTER(2,N) = CLUSTER(I)%MOM(2)
            MCLUSTER(3,N) = CLUSTER(I)%MOM(3)
          ENDDO
        ENDDO  !  I = 1, NCLUSTER
      ENDIF     
C-----------------------------------------------
      IF (NINDX > 0) THEN
        DO J=1,NINDX
#include "lockon.inc"
          WRITE(IOUT ,1000) CLUSTER(INDX(J))%ID
          WRITE(ISTDO,1100) CLUSTER(INDX(J))%ID,TT
#include "lockoff.inc"
        END DO
      ENDIF         
C-----------------------------------------------
 1000 FORMAT(5X,'DELETE ELEMENT CLUSTER,ID=',I10)
 1100 FORMAT(5X,'DELETE ELEMENT CLUSTER,ID=',I10,',  AT TIME ',1PE16.9)
C-----------
      RETURN
      END
