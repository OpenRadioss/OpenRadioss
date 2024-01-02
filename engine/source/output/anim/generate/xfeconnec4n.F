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
Chd|  XFECONNEC4N                   source/output/anim/generate/xfeconnec4n.F
Chd|-- called by -----------
Chd|        XFECUT                        source/output/anim/generate/xfecut.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE XFECONNEC4N(JFT     ,JLT     ,NFT   ,IXC    ,ELCUTC ,
     .                       IEL_CRK ,IADC_CRK,ILEV  ,NODEDGE,CRKEDGE,
     .                       XEDGE4N )
C----------------------------------------------- 
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT,NFT,ILEV,IXC(NIXC,*),ELCUTC(2,*),IEL_CRK(*),
     .        IADC_CRK(4,*),XEDGE4N(4,*),NODEDGE(2,*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,K1,K2,K3,K4,KK,p1,p2,NOD1,NOD2,IED0,IED1,IED2,                         
     .   IEDGE,IEDGE1,IEDGE2,EDGE,EDGE1,EDGE2,IXEL,ILAY,ITRI,ILV,
     .   FAC,OK,SN,ELCUT,ELCRK,IADC1,IADC2,IADC3,IADC4,NX1,NX2,NX3,NX4
      INTEGER IFI0(4,MVSIZ),POS(2),
     .   IED(4),IXI(4),D(4),DX(8),NX(MVSIZ)
      my_real 
     .   XINT,YINT,ZINT,XM,YM,ZM,X10,Y10,Z10,X20,Y20,Z20,BETA
      my_real 
     .   XIN(4,MVSIZ),YIN(4,MVSIZ),ZIN(4,MVSIZ),
     .   XX(4,MVSIZ),YY(4,MVSIZ),ZZ(4,MVSIZ)
c---------------------
      DATA D /2,3,4,1/
      DATA DX/1,2,3,4,1,2,3,4/      
C=======================================================================
c     Re-build phantom element connectivities
C-----------------------------------------------
      IXEL = MOD(ILEV-1, NXEL) + 1
      ILAY = (ILEV-IXEL)/NXEL  + 1
c
      DO I=JFT,JLT       
        XIN(1,I) = ZERO  
        YIN(1,I) = ZERO  
        ZIN(1,I) = ZERO  
        XIN(2,I) = ZERO  
        YIN(2,I) = ZERO  
        ZIN(2,I) = ZERO  
        XIN(3,I) = ZERO  
        YIN(3,I) = ZERO  
        ZIN(3,I) = ZERO  
        XIN(4,I) = ZERO  
        YIN(4,I) = ZERO  
        ZIN(4,I) = ZERO  
      END DO             
c-----------------
      DO I=JFT,JLT                              
        ELCRK = IEL_CRK(I+NFT)                  
        IADC1 = IADC_CRK(1,ELCRK)               
        IADC2 = IADC_CRK(2,ELCRK)               
        IADC3 = IADC_CRK(3,ELCRK)               
        IADC4 = IADC_CRK(4,ELCRK)               
C
        IFI0(1,I) = XFEM_PHANTOM(ILAY)%IFI(IADC1)  
        IFI0(2,I) = XFEM_PHANTOM(ILAY)%IFI(IADC2)  
        IFI0(3,I) = XFEM_PHANTOM(ILAY)%IFI(IADC3)  
        IFI0(4,I) = XFEM_PHANTOM(ILAY)%IFI(IADC4)  
C
        IFI0(1,I) = ISIGN(1,IFI0(1,I))          
        IFI0(2,I) = ISIGN(1,IFI0(2,I))          
        IFI0(3,I) = ISIGN(1,IFI0(3,I))          
        IFI0(4,I) = ISIGN(1,IFI0(4,I))
C--------------
c       Copy local phantom node coordinates (per ILEV)
C--------------
c       node 1:
        XX(1,I) = CRKAVX(ILEV)%X(1,IADC1)  
        YY(1,I) = CRKAVX(ILEV)%X(2,IADC1)  
        ZZ(1,I) = CRKAVX(ILEV)%X(3,IADC1)  
c       node 2:
        XX(2,I) = CRKAVX(ILEV)%X(1,IADC2)  
        YY(2,I) = CRKAVX(ILEV)%X(2,IADC2)  
        ZZ(2,I) = CRKAVX(ILEV)%X(3,IADC2)  
c       node 3:
        XX(3,I) = CRKAVX(ILEV)%X(1,IADC3)  
        YY(3,I) = CRKAVX(ILEV)%X(2,IADC3)  
        ZZ(3,I) = CRKAVX(ILEV)%X(3,IADC3)  
c       node 4:
        XX(4,I) = CRKAVX(ILEV)%X(1,IADC4)   
        YY(4,I) = CRKAVX(ILEV)%X(2,IADC4)   
        ZZ(4,I) = CRKAVX(ILEV)%X(3,IADC4)   
      END DO                              
c-----------------------------------------------
c     calculate intersection coordinates of cut edges : XIN, YIN, ZIN
c-----------------------------------------------
      DO I=JFT,JLT
        ELCRK = IEL_CRK(I+NFT)               ! xfem sys number
        ELCUT = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
        IF (ELCUT /= 0) THEN
          DO K=1,4
            IED0  = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)    ! = 0,1,2
            IF (IED0 > 0) THEN
              EDGE = XEDGE4N(K,ELCRK)  ! global xfem edge number 
              BETA = CRKEDGE(ILAY)%RATIO(EDGE)                   
              NOD1 = NODEDGE(1,EDGE)                   
              NOD2 = NODEDGE(2,EDGE)                   
              IF (NOD1 == IXC(K+1,I+NFT) .and.         
     .            NOD2 == IXC(d(K)+1,I+NFT)) THEN      
                p1 = K                                 
                p2 = d(K)                              
              ELSEIF (NOD2 == IXC(K+1,I+NFT) .and.     
     .                NOD1 == IXC(d(K)+1,I+NFT)) THEN  
                p1 = d(K)                              
                p2 = K                                 
              ENDIF                                    
              X10 = XX(p1,I)                           
              Y10 = YY(p1,I)                           
              Z10 = ZZ(p1,I)                           
              X20 = XX(p2,I)                           
              Y20 = YY(p2,I)                           
              Z20 = ZZ(p2,I)                           
              XIN(IED0,I) = X10 + BETA*(X20-X10)       
              YIN(IED0,I) = Y10 + BETA*(Y20-Y10)       
              ZIN(IED0,I) = Z10 + BETA*(Z20-Z10)       
            END IF                                     
          END DO
        END IF
      END DO
c
c-----------------------------------------------
c     main loop over elements
C     SIMPLE CRACKED ELEMENT         
C     only one crack inside element  
c-----------------------------------------------
      DO I=JFT,JLT
        IF (ELCUTC(1,I+NFT) == 0) CYCLE    ! element standard pas coupe         
c
        ELCRK = IEL_CRK(I+NFT)                          
        ELCUT = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
        IF (ELCUT == 0) CYCLE              ! element fantome pas coupe   
        ITRI = XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK) 
        NX1  = XFEM_PHANTOM(ILAY)%ITRI(2,ELCRK) 
        NX2  = DX(NX1+1)                                                            
        NX3  = DX(NX1+2)                                                            
        NX4  = DX(NX1+3)                                                            
c-------------------------------
        IF (ITRI == 0) THEN   ! element cut by two phantoms
c-------------------------------
          POS(1) = 0 
          POS(2) = 0                         
          FAC    = 0                         
c
          IF (IXEL == 1) THEN  ! first phantom (positif)
C---
            DO K=1,4
              IED0 = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)
              IF (IED0 > 0) THEN
                FAC = FAC + 1
                p1 = K
                p2 = d(p1)
                IF (IFI0(p1,I) < 0) POS(FAC) = p1  ! save negative nodes
                IF (IFI0(p2,I) < 0) POS(FAC) = p2
                IED(FAC) = p1
                IF (FAC == 2) EXIT
              ENDIF
            END DO
C---
            IF (POS(1) /= 0 .and. POS(2) /= 0) THEN
              DO K=1,2
                IEDGE = CRKEDGE(ILAY)%IEDGEC(IED(K),ELCRK)
                IF (IEDGE > 0) THEN
c                 move negative nodes to intersection position                
                  KK = CRKSHELL(ILEV)%XNODEL(POS(K),ELCRK)
                  KK = KK - CRKNOD(ILEV)%CRKNUMNODS * (ILEV-1)
                  CRKAVX(ILEV)%XX(1,KK) = XIN(IEDGE,I)
                  CRKAVX(ILEV)%XX(2,KK) = YIN(IEDGE,I)
                  CRKAVX(ILEV)%XX(3,KK) = ZIN(IEDGE,I)
                ENDIF
              END DO
            ENDIF
C---
          ELSE IF (IXEL == 2) THEN  ! second phantom (negatif)
C---
            DO K=1,4
              IED0 = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)
              IF (IED0 > 0) THEN
                FAC = FAC + 1
                p1 = K
                p2 = d(p1)
                IF (IFI0(p1,I) > 0) POS(FAC) = p1  ! save positive nodes
                IF (IFI0(p2,I) > 0) POS(FAC) = p2
                IED(FAC) = p1
                IF (FAC == 2) EXIT
              ENDIF
            END DO
C---
            IF (POS(1) /= 0 .and. POS(2) /= 0) THEN
              DO K=1,2
                IEDGE = CRKEDGE(ILAY)%IEDGEC(IED(K),ELCRK)
                IF (IEDGE > 0) THEN
c                 move positive nodes to intersection position                
                  KK = CRKSHELL(ILEV)%XNODEL(POS(K),ELCRK)
                  KK = KK - CRKNOD(ILEV)%CRKNUMNODS * (ILEV-1)
                  CRKAVX(ILEV)%XX(1,KK) = XIN(IEDGE,I)
                  CRKAVX(ILEV)%XX(2,KK) = YIN(IEDGE,I)
                  CRKAVX(ILEV)%XX(3,KK) = ZIN(IEDGE,I)
                ENDIF
              END DO
            ENDIF
C---
          ELSE IF (IXEL == 3) THEN  ! third phantom not actif
          ENDIF
          
c---------------------------------
        ELSEIF (ITRI < 0) THEN
c---------------------------------
          IED1 = NX1                                                               
          IED2 = NX4                                                               
          IADC1 = IADC_CRK(NX1,ELCRK)             
          IADC2 = IADC_CRK(NX2,ELCRK)             
          IADC3 = IADC_CRK(NX3,ELCRK)             
          IADC4 = IADC_CRK(NX4,ELCRK)             
c                                                                                   
          IEDGE1 = CRKEDGE(ILAY)%IEDGEC(IED1,ELCRK)                                 
          IEDGE2 = CRKEDGE(ILAY)%IEDGEC(IED2,ELCRK)                                 
          EDGE1  = XEDGE4N(IED1,ELCRK)  ! global xfem edge number    
          EDGE2  = XEDGE4N(IED2,ELCRK)  ! global xfem edge number    
c
          KK = CRKSHELL(ILEV)%XNODEL(NX1,ELCRK)         
          K1 = KK - CRKNOD(ILEV)%CRKNUMNODS * (ILEV-1)    
          KK = CRKSHELL(ILEV)%XNODEL(NX2,ELCRK)         
          K2 = KK - CRKNOD(ILEV)%CRKNUMNODS * (ILEV-1)    
          KK = CRKSHELL(ILEV)%XNODEL(NX3,ELCRK)         
          K3 = KK - CRKNOD(ILEV)%CRKNUMNODS * (ILEV-1)    
          KK = CRKSHELL(ILEV)%XNODEL(NX4,ELCRK)         
          K4 = KK - CRKNOD(ILEV)%CRKNUMNODS * (ILEV-1)    
c          print*,'  ELCRK,ELCUT,ITRI=',ELCRK,ELCUT,ITRI
c          print*,'  ILEV,NX1=',ILEV,NX1
c          print*,'  ied1,EDGE1=',ied1,EDGE1                          
c          print*,'  ied2,EDGE2=',ied2,EDGE2                          

c--------
          IF (IXEL == 1) THEN
c             NX1   ! unchanged
c             NX2 ->  intersec( Nx1, Nx2)  , edge1     
c             NX3 ->  intersec( Nx4, Nx1)       
c             NX4 ->  NX3       
            CRKAVX(ILEV)%XX(1,K2) = XIN(IEDGE1,I)
            CRKAVX(ILEV)%XX(2,K2) = YIN(IEDGE1,I)            
            CRKAVX(ILEV)%XX(3,K2) = ZIN(IEDGE1,I)            
            CRKAVX(ILEV)%XX(1,K3) = XIN(IEDGE2,I)            
            CRKAVX(ILEV)%XX(2,K3) = YIN(IEDGE2,I)            
            CRKAVX(ILEV)%XX(3,K3) = ZIN(IEDGE2,I)            
            CRKAVX(ILEV)%XX(1,K4) = CRKAVX(ILEV)%XX(1,K3)    
            CRKAVX(ILEV)%XX(2,K4) = CRKAVX(ILEV)%XX(2,K3)    
            CRKAVX(ILEV)%XX(3,K4) = CRKAVX(ILEV)%XX(3,K3)    
c
          ELSE IF (IXEL == 2) THEN
c             NX1 -> intersec( Nx1, Nx4)  
c             NX2 -> N3
c             NX3   ! unchanged
c             NX4   ! unchanged
              CRKAVX(ILEV)%XX(1,K1) = XIN(IEDGE2,I)          
              CRKAVX(ILEV)%XX(2,K1) = YIN(IEDGE2,I)          
              CRKAVX(ILEV)%XX(3,K1) = ZIN(IEDGE2,I)          
              CRKAVX(ILEV)%XX(1,K2) = CRKAVX(ILEV)%XX(1,K3)
              CRKAVX(ILEV)%XX(2,K2) = CRKAVX(ILEV)%XX(2,K3)
              CRKAVX(ILEV)%XX(3,K2) = CRKAVX(ILEV)%XX(3,K3)
c
          ELSE IF (IXEL == 3) THEN
c             NX1 -> intersec( Nx1, Nx2), edge1  
c             NX2   ! unchanged
c             NX3   ! unchanged
c             NX4  -> intersec( Nx4, Nx1), edge2, unchanged if IED2 = TIP
c
              CRKAVX(ILEV)%XX(1,K1) = XIN(IEDGE1,I)  
              CRKAVX(ILEV)%XX(2,K1) = YIN(IEDGE1,I)  
              CRKAVX(ILEV)%XX(3,K1) = ZIN(IEDGE1,I)
               
              CRKAVX(ILEV)%XX(1,K4) = XIN(IEDGE2,I)  
              CRKAVX(ILEV)%XX(2,K4) = YIN(IEDGE2,I)  
              CRKAVX(ILEV)%XX(3,K4) = ZIN(IEDGE2,I)  

c              print*,'NX1,K1=',NX1,k1
c              print*,'   x,y=',CRKAVX(ILEV)%XX(1,K1),CRKAVX(ILEV)%XX(2,K1)
c              print*,'NX2,K2=',NX2,K2
c              print*,'   x,y=',CRKAVX(ILEV)%XX(1,K2),CRKAVX(ILEV)%XX(2,K2)
c              print*,'NX3,K3=',NX3,K3
c              print*,'   x,y=',CRKAVX(ILEV)%XX(1,K3),CRKAVX(ILEV)%XX(2,K3)
c              print*,'NX4,K4=',NX4,K4
c              print*,'   x,y=',CRKAVX(ILEV)%XX(1,K4),CRKAVX(ILEV)%XX(2,K4)
c
          END IF  ! IXEL

c---------------------------------
        ELSEIF (ITRI > 0) THEN
c---------------------------------
          IED1 = NX1                                                               
          IED2 = NX4                                                               
          IADC1 = IADC_CRK(NX1,ELCRK)             
          IADC2 = IADC_CRK(NX2,ELCRK)             
          IADC3 = IADC_CRK(NX3,ELCRK)             
          IADC4 = IADC_CRK(NX4,ELCRK)             
c                                                                                   
          IEDGE1 = CRKEDGE(ILAY)%IEDGEC(IED1,ELCRK)                                 
          IEDGE2 = CRKEDGE(ILAY)%IEDGEC(IED2,ELCRK)                                 
          EDGE1  = XEDGE4N(IED1,ELCRK)  ! global xfem edge number    
          EDGE2  = XEDGE4N(IED2,ELCRK)  ! global xfem edge number    
c
          KK = CRKSHELL(ILEV)%XNODEL(NX1,ELCRK)         
          K1 = KK - CRKNOD(ILEV)%CRKNUMNODS * (ILEV-1)    
          KK = CRKSHELL(ILEV)%XNODEL(NX2,ELCRK)         
          K2 = KK - CRKNOD(ILEV)%CRKNUMNODS * (ILEV-1)    
          KK = CRKSHELL(ILEV)%XNODEL(NX3,ELCRK)         
          K3 = KK - CRKNOD(ILEV)%CRKNUMNODS * (ILEV-1)    
          KK = CRKSHELL(ILEV)%XNODEL(NX4,ELCRK)         
          K4 = KK - CRKNOD(ILEV)%CRKNUMNODS * (ILEV-1)    

c          print*,' '
c          print*,'  ELCRK,ELCUT,ITRI=',ELCRK,ELCUT,ITRI
c          print*,'  ILEV,NX1=',ILEV,NX1
c          print*,'  ied1,EDGE1=',ied1,EDGE1                          
c          print*,'  ied2,EDGE2=',ied2,EDGE2                          
c--------
          IF (IXEL == 1) THEN
c           NX1 -> intersec( Nx1, Nx2), edge1 = tip
c           NX2 = const  
c           NX3 = const  
c           NX4 -> N3    
            CRKAVX(ILEV)%XX(1,K1) = XIN(IEDGE1,I)
            CRKAVX(ILEV)%XX(2,K1) = YIN(IEDGE1,I)
            CRKAVX(ILEV)%XX(3,K1) = ZIN(IEDGE1,I)
            CRKAVX(ILEV)%XX(1,K4) = CRKAVX(ILEV)%XX(1,K3)    
            CRKAVX(ILEV)%XX(2,K4) = CRKAVX(ILEV)%XX(2,K3)    
            CRKAVX(ILEV)%XX(3,K4) = CRKAVX(ILEV)%XX(3,K3)
c
c--------
          ELSE IF (IXEL == 2) THEN
c           NX1 = const                                
c           NX2 ->  intersec( Nx1, Nx2) , edge1 = tip        
c           NX3=NX2 
c           NX4  -> intersec( Nx4, Nx1)  , edge2
            CRKAVX(ILEV)%XX(1,K2) = XIN(IEDGE1,I)      
            CRKAVX(ILEV)%XX(2,K2) = YIN(IEDGE1,I)      
            CRKAVX(ILEV)%XX(3,K2) = ZIN(IEDGE1,I)      
            CRKAVX(ILEV)%XX(1,K3) = CRKAVX(ILEV)%XX(1,K2)    
            CRKAVX(ILEV)%XX(2,K3) = CRKAVX(ILEV)%XX(2,K2)    
            CRKAVX(ILEV)%XX(3,K3) = CRKAVX(ILEV)%XX(3,K2)    
            CRKAVX(ILEV)%XX(1,K4) = XIN(IEDGE2,I)    
            CRKAVX(ILEV)%XX(2,K4) = YIN(IEDGE2,I)    
            CRKAVX(ILEV)%XX(3,K4) = ZIN(IEDGE2,I)    
c
c--------
          ELSE IF (IXEL == 3) THEN
c           NX1 ->  intersec( Nx4, Nx1) , edge2                 
c           NX2 ->  intersec( Nx1, Nx2) , edge1  (unchanged if Nx2 move) 
c           NX3 ->  unchanged                                          
c           NX4 ->  unchanged                                        

            CRKAVX(ILEV)%XX(1,K1) = XIN(IEDGE2,I)    
            CRKAVX(ILEV)%XX(2,K1) = YIN(IEDGE2,I)    
            CRKAVX(ILEV)%XX(3,K1) = ZIN(IEDGE2,I)
c            CRKAVX(ILEV)%XX(1,K2) = CRKAVX(ILEV-2)%XX(1,K1) 
c            CRKAVX(ILEV)%XX(2,K2) = CRKAVX(ILEV-2)%XX(2,K1) 
c            CRKAVX(ILEV)%XX(3,K2) = CRKAVX(ILEV-2)%XX(3,K1) 
            
            CRKAVX(ILEV)%XX(1,K2) = XIN(IEDGE1,I)    
            CRKAVX(ILEV)%XX(2,K2) = YIN(IEDGE1,I)    
            CRKAVX(ILEV)%XX(3,K2) = ZIN(IEDGE1,I)    
c
!                 print*,'NX1,K1=',NX1,k1
!                 print*,'   x,y=',CRKAVX(ILEV)%XX(1,K1),CRKAVX(ILEV)%XX(2,K1)
!                 print*,'NX2,K2=',NX2,K2
!                 print*,'   x,y=',CRKAVX(ILEV)%XX(1,K2),CRKAVX(ILEV)%XX(2,K2)
!                 print*,'NX3,K3=',NX3,K3
!                 print*,'   x,y=',CRKAVX(ILEV)%XX(1,K3),CRKAVX(ILEV)%XX(2,K3)
!                 print*,'NX4,K4=',NX4,K4
!                 print*,'   x,y=',CRKAVX(ILEV)%XX(1,K4),CRKAVX(ILEV)%XX(2,K4)

          END IF  ! IXEL
C---
        END IF    ! ITRI
C-----------------
      ENDDO       ! I=JFT,JLT
C-----------------
      RETURN
      END
