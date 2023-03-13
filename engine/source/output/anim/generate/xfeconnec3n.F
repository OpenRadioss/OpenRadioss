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
Chd|  XFECONNEC3N                   source/output/anim/generate/xfeconnec3n.F
Chd|-- called by -----------
Chd|        XFECUT                        source/output/anim/generate/xfecut.F
Chd|-- calls ---------------
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE XFECONNEC3N(
     .     JFT      ,JLT      ,NFT     ,IXTG        ,ELCUTC   ,
     .     IEL_CRKTG,IAD_CRKTG,ILEV    ,NODEDGE     ,CRKEDGE  ,
     .     XEDGE3N  )
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
      INTEGER JFT,JLT,NFT,ILEV,IXTG(NIXTG,*),ELCUTC(2,*),IEL_CRKTG(*),
     .        IAD_CRKTG(3,*),XEDGE3N(3,*),NODEDGE(2,*)
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,K1,K2,K3,KK,p,P1,P2,P3,IFI1,IFI2,
     .  EDGE,IEDGE1,IEDGE2,EDGE1,EDGE2,IED1,IED2,
     .  IADC1,IADC2,IADC3,ILAY,IXEL,ELCUT,ELCRK,ELCRKTG,
     .  IED0,IFI10,NOD1,NOD2,ITRI,NX1,NX2,NX3
      INTEGER IFI0(3,MVSIZ),NEG(2),D1(3),D2(3),DX(6)
      my_real
     .   XIN(3,MVSIZ),YIN(3,MVSIZ),ZIN(3,MVSIZ),
     .   XX(3,MVSIZ),YY(3,MVSIZ),ZZ(3,MVSIZ)
      my_real X10,Y10,Z10,X20,Y20,Z20,BETA
C-------------------
      DATA d1/2,3,1/
      DATA d2/3,1,2/
      DATA DX/1,2,3,1,2,3/        
c      DATA ED/1,3,2,1,3,2/
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
      END DO
C-----------------
      DO I=JFT,JLT
        ELCRKTG = IEL_CRKTG(I+NFT)
        IADC1 = IAD_CRKTG(1,ELCRKTG)
        IADC2 = IAD_CRKTG(2,ELCRKTG)
        IADC3 = IAD_CRKTG(3,ELCRKTG)
C
        IFI0(1,I) = XFEM_PHANTOM(ILAY)%IFI(IADC1)
        IFI0(2,I) = XFEM_PHANTOM(ILAY)%IFI(IADC2)
        IFI0(3,I) = XFEM_PHANTOM(ILAY)%IFI(IADC3)
C
        IFI0(1,I) = ISIGN(1,IFI0(1,I))
        IFI0(2,I) = ISIGN(1,IFI0(2,I))
        IFI0(3,I) = ISIGN(1,IFI0(3,I))
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
      END DO
c-----------------------------------------------
c     calculate intersection coordinates of cut edges : XIN, YIN, ZIN
c-----------------------------------------------
      DO I=JFT,JLT
        ELCRKTG = IEL_CRKTG(I+NFT)
        ELCRK   = ELCRKTG + ECRKXFEC
        ELCUT   = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
        IF (ELCUT /= 0) THEN
          DO K=1,3
            IED0 = CRKEDGE(ILAY)%IEDGETG(K,ELCRKTG)
            IF (IED0 > 0) THEN
              EDGE = XEDGE3N(K,ELCRKTG)
              BETA = CRKEDGE(ILAY)%RATIO(EDGE)        
              NOD1 = NODEDGE(1,EDGE)
              NOD2 = NODEDGE(2,EDGE)
              IF (NOD1 == IXTG(K+1,I+NFT) .and.             
     .            NOD2 == IXTG(d1(K)+1,I+NFT)) THEN          
                p1 = K                                      
                p2 = d1(K)                                   
              ELSEIF (NOD2 == IXTG(K+1,I+NFT).and.          
     .                NOD1 == IXTG(d1(K)+1,I+NFT)) THEN      
                p1 = d1(K)                                   
                p2 = K                                      
              ENDIF
              X10 = XX(p1,I)                                
              Y10 = YY(p1,I)                                
              Z10 = ZZ(p1,I)                                
              X20 = XX(p2,I)                                
              Y20 = YY(p2,I)                                
              Z20 = ZZ(p2,I)                                
              XIN(IED0,I) = X10+BETA*(X20-X10)  
              YIN(IED0,I) = Y10+BETA*(Y20-Y10)  
              ZIN(IED0,I) = Z10+BETA*(Z20-Z10)  
            END IF                              
          END DO
        END IF
      END DO
c-----------------------------------------------
c     main loop over elements
C     SIMPLE CRACKED ELEMENT         
C     only one crack inside element  
c-----------------------------------------------
        DO I=JFT,JLT
          ELCRKTG = IEL_CRKTG(I+NFT)
          ELCRK   = ELCRKTG + ECRKXFEC
          ELCUT   = XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)
C---
          IF (ELCUTC(1,I+NFT) == 0) CYCLE
          p1 = 0
          p2 = 0
          p3 = 0
          DO K=1,3
            IFI10 = IFI0(K,I)
            IFI1  = IFI0(d1(K),I)
            IFI2  = IFI0(d2(K),I)
            IF (IFI10*IFI1 < 0 .and. IFI10*IFI2 < 0) THEN
              p1 = K
              p2 = d1(K)
              p3 = d2(K)
              EXIT
            END IF
          END DO
C
          IF (p1==0 .or. p2==0 .or. p3==0) CYCLE
C--------------------------
          ITRI   = XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK) 
          NX1    = XFEM_PHANTOM(ILAY)%ITRI(2,ELCRK) 
          NX2    = DX(NX1+1)                                                        
          NX3    = DX(NX1+2)                                                        
          IED1   = NX1                                                               
          IED2   = NX3                                                               
          IEDGE1 = CRKEDGE(ILAY)%IEDGETG(IED1,ELCRKTG)                                 
          IEDGE2 = CRKEDGE(ILAY)%IEDGETG(IED2,ELCRKTG)  
          EDGE1  = XEDGE3N(IED1,ELCRKTG)  ! global xfem edge number      
          EDGE2  = XEDGE3N(IED2,ELCRKTG)  ! global xfem edge number      
c
          KK = CRKSHELL(ILEV)%XNODEL(NX1,ELCRK)         
          K1 = KK - CRKNOD(ILEV)%CRKNUMNODS * (ILEV-1)    
          KK = CRKSHELL(ILEV)%XNODEL(NX2,ELCRK)         
          K2 = KK - CRKNOD(ILEV)%CRKNUMNODS * (ILEV-1)    
          KK = CRKSHELL(ILEV)%XNODEL(NX3,ELCRK)         
          K3 = KK - CRKNOD(ILEV)%CRKNUMNODS * (ILEV-1)    
C--------------------------
          IF (ITRI < 0) THEN
C--------------------------
            IF (IXEL == 1) THEN
c             NX1 -> unchanged
c             NX2 -> intersec edge1 : Nx1->Nx2
c             NX3 -> intersec edge2 : Nx3->Nx1       
c
              CRKAVX(ILEV)%XX(1,K2) = XIN(IEDGE1,I)
              CRKAVX(ILEV)%XX(2,K2) = YIN(IEDGE1,I)          
              CRKAVX(ILEV)%XX(3,K2) = ZIN(IEDGE1,I)          
              CRKAVX(ILEV)%XX(1,K3) = XIN(IEDGE2,I)          
              CRKAVX(ILEV)%XX(2,K3) = YIN(IEDGE2,I)          
              CRKAVX(ILEV)%XX(3,K3) = ZIN(IEDGE2,I)          
c
            ELSEIF (IXEL == 2) THEN
c             NX1 -> intersec edge2 : Nx3->Nx1
c             NX2 -> unchanged
c             NX3 -> unchanged
c
              CRKAVX(ILEV)%XX(1,K1) = XIN(IEDGE2,I)
              CRKAVX(ILEV)%XX(2,K1) = YIN(IEDGE2,I)
              CRKAVX(ILEV)%XX(3,K1) = ZIN(IEDGE2,I)
c
            ELSEIF (IXEL == 3) THEN
c             NX1 -> intersec edge1 : Nx1->Nx2
c             NX2 -> unchanged
c             NX3 -> moved
              CRKAVX(ILEV)%XX(1,K1) = XIN(IEDGE1,I)
              CRKAVX(ILEV)%XX(2,K1) = YIN(IEDGE1,I)
              CRKAVX(ILEV)%XX(3,K1) = ZIN(IEDGE1,I)
               
              CRKAVX(ILEV)%XX(1,K3) = XIN(IEDGE2,I)  
              CRKAVX(ILEV)%XX(2,K3) = YIN(IEDGE2,I)  
              CRKAVX(ILEV)%XX(3,K3) = ZIN(IEDGE2,I)  
            END IF
C--------------------------
          ELSEIF (ITRI > 0) THEN
C--------------------------
            IF (IXEL == 1) THEN
c             NX1 -> intersec edge1 : Nx1->Nx2
c             NX2 -> unchanged
c             NX3 -> unchanged  
c                                                                                   
              CRKAVX(ILEV)%XX(1,K1) = XIN(IEDGE1,I)
              CRKAVX(ILEV)%XX(2,K1) = YIN(IEDGE1,I)
              CRKAVX(ILEV)%XX(3,K1) = ZIN(IEDGE1,I)
c
            ELSEIF (IXEL == 2) THEN
c             NX1 -> unchanged
c             NX2 -> intersec edge1 : Nx1->Nx2
c             NX3 -> intersec edge2 : Nx3->Nx1  
              CRKAVX(ILEV)%XX(1,K2) = XIN(IEDGE1,I)
              CRKAVX(ILEV)%XX(2,K2) = YIN(IEDGE1,I)
              CRKAVX(ILEV)%XX(3,K2) = ZIN(IEDGE1,I)
              CRKAVX(ILEV)%XX(1,K3) = XIN(IEDGE2,I)
              CRKAVX(ILEV)%XX(2,K3) = YIN(IEDGE2,I)
              CRKAVX(ILEV)%XX(3,K3) = ZIN(IEDGE2,I)
            ELSEIF (IXEL == 3) THEN
c             NX1 -> intersec edge2 : Nx3->Nx1
c             NX2 -> moved
c             NX3 -> unchanged  
              CRKAVX(ILEV)%XX(1,K1) = XIN(IEDGE2,I)
              CRKAVX(ILEV)%XX(2,K1) = YIN(IEDGE2,I)
              CRKAVX(ILEV)%XX(3,K1) = ZIN(IEDGE2,I)
c
              CRKAVX(ILEV)%XX(1,K2) = CRKAVX(ILEV-2)%XX(1,K1) 
              CRKAVX(ILEV)%XX(2,K2) = CRKAVX(ILEV-2)%XX(2,K1) 
              CRKAVX(ILEV)%XX(3,K2) = CRKAVX(ILEV-2)%XX(3,K1) 
c              CRKAVX(ILEV)%XX(1,K2) = XIN(IEDGE1,I)    
c              CRKAVX(ILEV)%XX(2,K2) = YIN(IEDGE1,I)    
c              CRKAVX(ILEV)%XX(3,K2) = ZIN(IEDGE1,I)    
            END IF ! IXEL
C---
          ENDIF    ! ITRI 
C-----------------
        ENDDO      ! I=JFT,JLT
C-----------------
      RETURN
      END
