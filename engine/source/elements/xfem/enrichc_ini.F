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
Chd|  ENRICHC_INI                   source/elements/xfem/enrichc_ini.F
Chd|-- called by -----------
Chd|        INIXFEM                       source/elements/xfem/inixfem.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        LSINT4                        source/elements/xfem/crklayer4n_adv.F
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ENRICHC_INI(ELBUF_STR,XFEM_STR,
     .      IXC      ,NFT     ,JFT     ,JLT    ,NXLAY   ,
     .      IADC_CRK ,IEL_CRK ,INOD_CRK,ELCUTC ,NODEDGE ,
     .      CRKNODIAD,KNOD2ELC,X       ,CRKEDGE,XEDGE4N )
C-----------------------------------------------
      USE CRACKXFEM_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFT,JFT,JLT,NXLAY
      INTEGER IXC(NIXC,*),INOD_CRK(*),KNOD2ELC(*),IADC_CRK(4,*),
     .  IEL_CRK(*),ELCUTC(2,*),NODEDGE(2,*),CRKNODIAD(*),XEDGE4N(4,*)
      my_real X(3,*)
      TYPE (ELBUF_STRUCT_)                  :: ELBUF_STR
      TYPE (ELBUF_STRUCT_), DIMENSION(NXEL) :: XFEM_STR
      TYPE (XFEM_EDGE_)   , DIMENSION(*)    :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ELCRK,ILEV,ELCUT,pp1,pp2,pp3,IADC(4),IENR0(4),
     . IENR(4),IED,IEDGE,r,IE10,IE20,IE1,IE2,NOD1,NOD2,N(4),NX(4),
     . DD(4),ISIGN1,ISIGN2,ISIGN3,ISIGN4,IAD1,IAD2,IAD3,IAD4,
     . ISIGN0(NXEL,4),p1,p2,LAYCUT,ICUTEDGE,IBOUNDEDGE,
     . NTAG(4),EDGEENR(4),ENR(4),
     . ILAY,ITRI,NX1,NX2,NX3,NX4,NM,NP
      my_real
     .   X1G(MVSIZ),X2G(MVSIZ),X3G(MVSIZ),X4G(MVSIZ),
     .   Y1G(MVSIZ),Y2G(MVSIZ),Y3G(MVSIZ),Y4G(MVSIZ),
     .   Z1G(MVSIZ),Z2G(MVSIZ),Z3G(MVSIZ),Z4G(MVSIZ),AREA(MVSIZ),
     .   LXYZ0(3),RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),
     .   SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),R11(MVSIZ),R12(MVSIZ),
     .   R13(MVSIZ),R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),R31(MVSIZ),
     .   R32(MVSIZ),R33(MVSIZ),XL1(MVSIZ),YL1(MVSIZ),XL2(MVSIZ),
     .   YL2(MVSIZ),XL3(MVSIZ),YL3(MVSIZ),XL4(MVSIZ),YL4(MVSIZ),
     .   FIT(4,MVSIZ),OFFG(MVSIZ),XIN(2,MVSIZ),YIN(2,MVSIZ),
     .   XXL(4,MVSIZ),YYL(4,MVSIZ),XN(4),YN(4),DX(8),XM(2),YM(2)
      my_real XXX,YYY,ZZZ,FI,BETA,X10,Y10,Z10,X20,Y20,Z20,
     .   X1,Y1,X2,Y2,X3,Y3,X4,Y4,AREA1,AREA2,AREA3
      DATA DD/2,3,4,1/
      DATA DX/1,2,3,4,1,2,3,4/
C=======================================================================
C     COORDONNEES (GLOBAL)
C-----------------------
      DO I=JFT,JLT
        X1G(I)=X(1,IXC(2,I+NFT))
        Y1G(I)=X(2,IXC(2,I+NFT))
        Z1G(I)=X(3,IXC(2,I+NFT))
        X2G(I)=X(1,IXC(3,I+NFT))
        Y2G(I)=X(2,IXC(3,I+NFT))
        Z2G(I)=X(3,IXC(3,I+NFT))
        X3G(I)=X(1,IXC(4,I+NFT))
        Y3G(I)=X(2,IXC(4,I+NFT))
        Z3G(I)=X(3,IXC(4,I+NFT))
        X4G(I)=X(1,IXC(5,I+NFT))
        Y4G(I)=X(2,IXC(5,I+NFT))
        Z4G(I)=X(3,IXC(5,I+NFT))
      ENDDO
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      DO I=JFT,JLT
        RX(I) = X2G(I)+X3G(I)-X1G(I)-X4G(I)
        SX(I) = X3G(I)+X4G(I)-X1G(I)-X2G(I)
        RY(I) = Y2G(I)+Y3G(I)-Y1G(I)-Y4G(I)
        SY(I) = Y3G(I)+Y4G(I)-Y1G(I)-Y2G(I)
        RZ(I) = Z2G(I)+Z3G(I)-Z1G(I)-Z4G(I)
        SZ(I) = Z3G(I)+Z4G(I)-Z1G(I)-Z2G(I)
        OFFG(I) = ELBUF_STR%GBUF%OFF(I)
      ENDDO
      K = 0
      CALL CLSKEW3(JFT,JLT,K,
     .   RX, RY, RZ,
     .   SX, SY, SZ,
     .   R11,R12,R13,R21,R22,R23,R31,R32,R33,AREA,OFFG )
C--------------------------
C     GLOBAL-->LOCAL TRANSFORMATION
C--------------------------
      DO I=JFT,JLT
        LXYZ0(1)=QUART*(X3G(I)+X4G(I)+X1G(I)+X2G(I))
        LXYZ0(2)=QUART*(Y3G(I)+Y4G(I)+Y1G(I)+Y2G(I))
        LXYZ0(3)=QUART*(Z3G(I)+Z4G(I)+Z1G(I)+Z2G(I))
        XXX = X1G(I)-LXYZ0(1)
        YYY = Y1G(I)-LXYZ0(2)
        ZZZ = Z1G(I)-LXYZ0(3)
        XL1(I)=R11(I)*XXX+R21(I)*YYY+R31(I)*ZZZ
        YL1(I)=R12(I)*XXX+R22(I)*YYY+R32(I)*ZZZ
        XXX = X2G(I)-LXYZ0(1)
        YYY = Y2G(I)-LXYZ0(2)
        ZZZ = Z2G(I)-LXYZ0(3)
        XL2(I)=R11(I)*XXX+R21(I)*YYY+R31(I)*ZZZ
        YL2(I)=R12(I)*XXX+R22(I)*YYY+R32(I)*ZZZ
        XXX = X3G(I)-LXYZ0(1)
        YYY = Y3G(I)-LXYZ0(2)
        ZZZ = Z3G(I)-LXYZ0(3)
        XL3(I)=R11(I)*XXX+R21(I)*YYY+R31(I)*ZZZ
        YL3(I)=R12(I)*XXX+R22(I)*YYY+R32(I)*ZZZ
        XXX = X4G(I)-LXYZ0(1)
        YYY = Y4G(I)-LXYZ0(2)
        ZZZ = Z4G(I)-LXYZ0(3)
        XL4(I)=R11(I)*XXX+R21(I)*YYY+R31(I)*ZZZ
        YL4(I)=R12(I)*XXX+R22(I)*YYY+R32(I)*ZZZ
        AREA(I) = QUART*AREA(I)
      ENDDO
c-----------------------------------------------------
c     Loop over layers
c-----------------------------------------------------
      DO ILAY=1,NXLAY
C       sub-levels (sub-phantom elements) within ILAY
        pp1 = NXEL*(ILAY-1)+1
        pp2 = pp1 + 1
        pp3 = pp2 + 1
C
        DO I=JFT,JLT
          FIT(1,I)=ZERO
          FIT(2,I)=ZERO
          FIT(3,I)=ZERO
          FIT(4,I)=ZERO
        ENDDO
C
        DO I=JFT,JLT
          ELCRK = IEL_CRK(I+NFT)
          LAYCUT = CRKEDGE(ILAY)%LAYCUT(ELCRK)
          IF (LAYCUT /= 0) THEN
            XN(1)   = XL1(I)
            YN(1)   = YL1(I)
            XN(2)   = XL2(I)
            YN(2)   = YL2(I)
            XN(3)   = XL3(I)
            YN(3)   = YL3(I)
            XN(4)   = XL4(I)
            YN(4)   = YL4(I)
            XXL(1,I)= XL1(I)
            YYL(1,I)= YL1(I)
            XXL(2,I)= XL2(I)
            YYL(2,I)= YL2(I)
            XXL(3,I)= XL3(I)
            YYL(3,I)= YL3(I)
            XXL(4,I)= XL4(I)
            YYL(4,I)= YL4(I)
            DO K=1,4
              p1 = K
              p2 = dd(K)
              IED = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)
              IF (IED > 0) THEN
                IEDGE = XEDGE4N(K,ELCRK)
                BETA  = CRKEDGE(ILAY)%RATIO(IEDGE)
                NOD1  = NODEDGE(1,IEDGE)   
                NOD2  = NODEDGE(2,IEDGE)              
                IF (NOD1 == IXC(K+1,I+NFT) .and. NOD2 == IXC(dd(K)+1,I+NFT)) THEN
                  p1 = K
                  p2 = dd(K)
                ELSEIF (NOD2 == IXC(K+1,I+NFT).and.NOD1 == IXC(dd(K)+1,I+NFT))THEN
                  p1 = dd(K)
                  p2 = K
                ENDIF
                XIN(IED,I) = XN(P1) + BETA*(XN(P2) - XN(P1))
                YIN(IED,I) = YN(P1) + BETA*(YN(P2) - YN(P1)) 
                XM(IED) = UNDEMI*(XN(p1)+XN(p2))
                YM(IED) = UNDEMI*(YN(p1)+YN(p2))
              ENDIF
            ENDDO
C
            DO K=1,4
              FI=ZERO
              CALL LSINT4(XM(1),YM(1),XM(2),YM(2),XN(K),YN(K),FI )
              IF (FIT(K,I)==ZERO) FIT(K,I) = FI
            ENDDO
          ENDIF
        ENDDO
C
        IF (ILAY == 1) THEN
          DO I=JFT,JLT
            ELCRK = IEL_CRK(I+NFT)
            ELCUT = CRKEDGE(ILAY)%LAYCUT(ELCRK)
            IF (ELCUT /= 0) THEN
              ELCUTC(1,I+NFT) = 1
              ELCUTC(2,I+NFT) = 1
            ENDIF
          ENDDO
        ENDIF
c
c-------
c
        DO I=JFT,JLT
          ELCRK  = IEL_CRK(I+NFT)
          LAYCUT = CRKEDGE(ILAY)%LAYCUT(ELCRK)
          IF (LAYCUT /= 0) THEN
c
          IADC(1) = IADC_CRK(1,ELCRK)
          IADC(2) = IADC_CRK(2,ELCRK)
          IADC(3) = IADC_CRK(3,ELCRK)
          IADC(4) = IADC_CRK(4,ELCRK)
C
          IENR0(1) = CRKNODIAD(IADC(1))
          IENR0(2) = CRKNODIAD(IADC(2))
          IENR0(3) = CRKNODIAD(IADC(3))
          IENR0(4) = CRKNODIAD(IADC(4))
C
          N(1) = IXC(2,I+NFT)
          N(2) = IXC(3,I+NFT)
          N(3) = IXC(4,I+NFT)
          N(4) = IXC(5,I+NFT)
C
          NX(1) = INOD_CRK(N(1))
          NX(2) = INOD_CRK(N(2))
          NX(3) = INOD_CRK(N(3))
          NX(4) = INOD_CRK(N(4))
C
          IENR(1)  = IENR0(1) + KNOD2ELC(NX(1))*(ILAY-1)
          IENR(2)  = IENR0(2) + KNOD2ELC(NX(2))*(ILAY-1)
          IENR(3)  = IENR0(3) + KNOD2ELC(NX(3))*(ILAY-1)
          IENR(4)  = IENR0(4) + KNOD2ELC(NX(4))*(ILAY-1)
C
          NTAG(1:4)    = 0
          EDGEENR(1:4) = 0
          ENR(1:4)     = 0
C
          DO r=1,4
            IED = CRKEDGE(ILAY)%IEDGEC(r,ELCRK)
            IF (IED > 0) THEN
              NTAG(r) = NTAG(r) + 1
              NTAG(dd(r)) = NTAG(dd(r)) + 1
c
              IEDGE = XEDGE4N(r,ELCRK)
              NOD1  = NODEDGE(1,IEDGE)
              NOD2  = NODEDGE(2,IEDGE)
              IE10  = CRKEDGE(ILAY)%EDGEENR(1,IEDGE)
              IE20  = CRKEDGE(ILAY)%EDGEENR(2,IEDGE)
              IF (NOD1 == N(r) .and. NOD2 == N(dd(r))) THEN
                p1 = r
                p2 = dd(r)
              ELSEIF (NOD2 == N(r) .and. NOD1 == N(dd(r))) THEN
                p1 = dd(r)
                p2 = r
              ENDIF
              EDGEENR(p1) = IE10
              EDGEENR(p2) = IE20
            ENDIF
          ENDDO
C
          DO r=1,4
            IF(NTAG(r) > 0)THEN
              ENR(r) = EDGEENR(r)
            ELSE
              ENR(r) = IENR(r)
            ENDIF
          ENDDO
C
          DO r=1,4
            IF (IENR(r) > IENRNOD) THEN
               WRITE(IOUT,*) 'ERROR CRACK INITIATION,ENRICHMENT NODE EXCEEDED'
              CALL ARRET(2)
            ENDIF
          ENDDO
C
          ISIGN1 = INT(SIGN(UN,FIT(1,I)))
          ISIGN2 = INT(SIGN(UN,FIT(2,I)))
          ISIGN3 = INT(SIGN(UN,FIT(3,I)))
          ISIGN4 = INT(SIGN(UN,FIT(4,I)))
C
          IF (FIT(1,I) == ZERO) ISIGN1 = 0
          IF (FIT(2,I) == ZERO) ISIGN2 = 0
          IF (FIT(3,I) == ZERO) ISIGN3 = 0
          IF (FIT(4,I) == ZERO) ISIGN4 = 0
C
          DO J=1,NXEL
            ISIGN0(J,1) = ISIGN1
            ISIGN0(J,2) = ISIGN2
            ISIGN0(J,3) = ISIGN3
            ISIGN0(J,4) = ISIGN4
          ENDDO
c---------------------------------------------------          
          DO K=1,4
            IED = CRKEDGE(ILAY)%IEDGEC(K,ELCRK)
            IF (IED > 0) THEN
              IEDGE = XEDGE4N(K,ELCRK)
              NOD1  = NODEDGE(1,IEDGE)
              NOD2  = NODEDGE(2,IEDGE)
              IF (NOD1 == N(K) .and. NOD2 == N(dd(K))) THEN
                p1 = K
                p2 = dd(K)
              ELSEIF (NOD2 == N(K) .and. NOD1 == N(dd(K))) THEN
                p1 = dd(K)
                p2 = K
              ENDIF
              ICUTEDGE   = CRKEDGE(ILAY)%ICUTEDGE(IEDGE)
              IBOUNDEDGE = CRKEDGE(ILAY)%IBORDEDGE(IEDGE)
              IF (ICUTEDGE == 2 .AND. IBOUNDEDGE == 0) THEN
                ENR(p1) = -ENR(p1)
                ENR(p2) = -ENR(p2)
              ENDIF
            ENDIF
          ENDDO
c---------------------------------------------------          
          ITRI = 0
          NM   = 0    
          NP   = 0    
          DO K=1,4
            IF (ISIGN0(1,K) > 0) THEN
              ITRI = ITRI + 1
              NP     = K
            ELSEIF (ISIGN0(1,K) < 0) THEN
              NM     = K
            ENDIF
          ENDDO
c---   
          IF (ITRI == 1) THEN
            ITRI = -1
            NX1  = NP
          ELSEIF (ITRI == 3) THEN
            ITRI = 1
            NX1  = NM
          ELSEIF (ITRI == 2) THEN
            ITRI = 0
            NX1  = NP  
            IF (NP > 0 .and. ISIGN0(1,NP-1) > 0)  THEN  
              NX1 = NP-1
            ELSE
              NX1 = NP
            ENDIF
          ENDIF
c--- 
          NX2  = DX(NX1+1)    
          NX3  = DX(NX1+2)    
          NX4  = DX(NX1+3)    
          IAD1 = IADC(NX1)    
          IAD2 = IADC(NX2)    
          IAD3 = IADC(NX3)    
          IAD4 = IADC(NX4)    
          XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK) = ITRI
          XFEM_PHANTOM(ILAY)%ITRI(2,ELCRK) = NX1  
c--------------------------          
c       phantom area factors
c--------------------------          
        IF (ITRI < 0) THEN                                     
c--       IXEL=2                                      
          IED = CRKEDGE(ILAY)%IEDGEC(NX4,ELCRK)                        
          X1  = XIN(IED,I)                                           
          Y1  = YIN(IED,I)                                           
c
          X2  = XXL(NX3,I)                                           
          Y2  = YYL(NX3,I)                                           
          X3  = XXL(NX4,I)                                           
          Y3  = YYL(NX4,I)                                           
          AREA2 = UNDEMI*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))  
c--       IXEL=1 AREA factor                                           
          IED = CRKEDGE(ILAY)%IEDGEC(NX1,ELCRK)                        
          X2  = XIN(IED,I)                                             
          Y2  = YIN(IED,I)                                             
          X3  = XXL(NX1,I)                                             
          Y3  = YYL(NX1,I)                                             
          AREA1 = UNDEMI*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))        
          AREA1 = AREA1 / AREA(I)                                    
          AREA2 = AREA2 / AREA(I)                                    
          AREA3 = UN - AREA1 - AREA2                                 
        ELSEIF (ITRI > 0) THEN                                     
c--       IXEL=1                                     
          IED = CRKEDGE(ILAY)%IEDGEC(NX1,ELCRK)                        
          X1  = XIN(IED,I)                                           
          Y1  = YIN(IED,I)                                           
c
          X2  = XXL(NX2,I)                                           
          Y2  = YYL(NX2,I)                                           
          X3  = XXL(NX3,I)                                           
          Y3  = YYL(NX3,I)                                           
          AREA1 = UNDEMI*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))      
c--       IXEL=2                                       
          IED = CRKEDGE(ILAY)%IEDGEC(NX4,ELCRK)                        
          X2  = XIN(IED,I)                                           
          Y2  = YIN(IED,I)                                           
          X3  = XXL(NX1,I)                                           
          Y3  = YYL(NX1,I)                                           
          AREA2 = UNDEMI*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))      
c
          AREA1 = AREA1 / AREA(I)                                    
          AREA2 = AREA2 / AREA(I)                                    
          AREA3 = UN - AREA1 - AREA2                                 
          CRKLVSET(PP1)%AREA(ELCRK) = AREA1                          
          CRKLVSET(PP2)%AREA(ELCRK) = AREA2                          
          CRKLVSET(PP3)%AREA(ELCRK) = AREA3                          
        ELSE   ! ITRI == 0         
          X1  = XXL(NX1,I)        
          Y1  = YYL(NX1,I)                       
          X2  = XXL(NX2,I)        
          Y2  = YYL(NX2,I)
          IED = CRKEDGE(ILAY)%IEDGEC(NX2,ELCRK)
          IF (IED > 0) THEN     
            X3 = XIN(IED,I)     
            Y3 = YIN(IED,I)
          ELSE
            ! print*,' ERROR: K,IED=',NX2,IED
          ENDIF
          IED = CRKEDGE(ILAY)%IEDGEC(NX4,ELCRK)
          IF (IED > 0) THEN     
            X4 = XIN(IED,I)     
            Y4 = YIN(IED,I)
          ELSE
            ! print*,' ERROR: K,IED=',NX4,IED
          ENDIF
          AREA1 = UNDEMI*ABS(X1*Y2 - X2*Y1 + X2*Y3 - X3*Y2 +  
     .                       X3*Y4 - X4*Y3 + X4*Y1 - X1*Y4)     
          AREA1 = AREA1 / AREA(I)
          AREA2 = UN - AREA1
          AREA3 = ZERO
        ENDIF
c
        CRKLVSET(PP1)%AREA(ELCRK) = AREA1    
        CRKLVSET(PP2)%AREA(ELCRK) = AREA2    
        CRKLVSET(PP3)%AREA(ELCRK) = AREA3    
c---------------------------------------------------          
c         First phantom element within ILAY (positif)
          ILEV = pp1
C------------------
C
          IF (ITRI == 0) THEN
            CRKLVSET(ILEV)%ENR0(1,IADC(1)) = ABS(ENR(1)) 
            CRKLVSET(ILEV)%ENR0(1,IADC(2)) = ENR(2)
            CRKLVSET(ILEV)%ENR0(1,IADC(3)) = ENR(3)
            CRKLVSET(ILEV)%ENR0(1,IADC(4)) = ENR(4)
          ELSE
            CRKLVSET(ILEV)%ENR0(1,IADC(1)) = ENR(1)
            CRKLVSET(ILEV)%ENR0(1,IADC(2)) = ENR(2)
            CRKLVSET(ILEV)%ENR0(1,IADC(3)) = ENR(3)
            CRKLVSET(ILEV)%ENR0(1,IADC(4)) = ENR(4)
          ENDIF
C
          IF(ISIGN0(1,1) > 0) CRKLVSET(ILEV)%ENR0(1,IADC(1)) = 0
          IF(ISIGN0(1,2) > 0) CRKLVSET(ILEV)%ENR0(1,IADC(2)) = 0
          IF(ISIGN0(1,3) > 0) CRKLVSET(ILEV)%ENR0(1,IADC(3)) = 0
          IF(ISIGN0(1,4) > 0) CRKLVSET(ILEV)%ENR0(1,IADC(4)) = 0
C
          CRKLVSET(ILEV)%ENR0(2,IADC(1)) = CRKLVSET(ILEV)%ENR0(1,IADC(1))
          CRKLVSET(ILEV)%ENR0(2,IADC(2)) = CRKLVSET(ILEV)%ENR0(1,IADC(2))
          CRKLVSET(ILEV)%ENR0(2,IADC(3)) = CRKLVSET(ILEV)%ENR0(1,IADC(3))
          CRKLVSET(ILEV)%ENR0(2,IADC(4)) = CRKLVSET(ILEV)%ENR0(1,IADC(4))
C
          XFEM_PHANTOM(ILAY)%IFI(IADC(1)) = ISIGN0(1,1)
          XFEM_PHANTOM(ILAY)%IFI(IADC(2)) = ISIGN0(1,2)
          XFEM_PHANTOM(ILAY)%IFI(IADC(3)) = ISIGN0(1,3)
          XFEM_PHANTOM(ILAY)%IFI(IADC(4)) = ISIGN0(1,4)
C------------------
c         Second phantom element within ILAY (negatif)
          ILEV = pp2
C------------------
C 
          CRKLVSET(ILEV)%ENR0(1,IADC(1)) = ENR(1)
          CRKLVSET(ILEV)%ENR0(1,IADC(2)) = ENR(2)
          CRKLVSET(ILEV)%ENR0(1,IADC(3)) = ENR(3)
          CRKLVSET(ILEV)%ENR0(1,IADC(4)) = ENR(4)
C
          IF(ISIGN0(2,1) < 0) CRKLVSET(ILEV)%ENR0(1,IADC(1)) = 0
          IF(ISIGN0(2,2) < 0) CRKLVSET(ILEV)%ENR0(1,IADC(2)) = 0
          IF(ISIGN0(2,3) < 0) CRKLVSET(ILEV)%ENR0(1,IADC(3)) = 0
          IF(ISIGN0(2,4) < 0) CRKLVSET(ILEV)%ENR0(1,IADC(4)) = 0
C
          CRKLVSET(ILEV)%ENR0(2,IADC(1)) = CRKLVSET(ILEV)%ENR0(1,IADC(1))
          CRKLVSET(ILEV)%ENR0(2,IADC(2)) = CRKLVSET(ILEV)%ENR0(1,IADC(2))
          CRKLVSET(ILEV)%ENR0(2,IADC(3)) = CRKLVSET(ILEV)%ENR0(1,IADC(3))
          CRKLVSET(ILEV)%ENR0(2,IADC(4)) = CRKLVSET(ILEV)%ENR0(1,IADC(4))
C------------------
c         Third phantom element within ILAY
          ILEV = pp3
C------------------
          IF (ITRI < 0) THEN   !  sign ILEV3 = ILEV2 < 0
              IE1 = XEDGE4N(NX2,ELCRK)
              IE2 = XEDGE4N(NX4,ELCRK)
              IF (CRKEDGE(ILAY)%ICUTEDGE(IE2) == 2) THEN
                CRKLVSET(pp3)%ENR0(1,IAD1) = ABS(CRKLVSET(pp2)%ENR0(1,IAD1))
                CRKLVSET(pp3)%ENR0(1,IAD2) = CRKLVSET(pp2)%ENR0(1,IAD2)
                CRKLVSET(pp3)%ENR0(1,IAD3) = CRKLVSET(pp2)%ENR0(1,IAD3)
                CRKLVSET(pp3)%ENR0(1,IAD4) = CRKLVSET(pp2)%ENR0(1,IAD4)
c
              ELSEIF (CRKEDGE(ILAY)%ICUTEDGE(IE1) == 2) THEN  
                 !  print*,' IMPOSSIBLE CASE'
              ENDIF
c-------
            ELSEIF (ITRI > 0) THEN      ! ITRI=1,   sign ILEV3 = ILEV1
c-------
              IE1 = XEDGE4N(NX1,ELCRK)
              IE2 = XEDGE4N(NX4,ELCRK)
              IF (CRKEDGE(ILAY)%ICUTEDGE(IE1) == 2) THEN
                CRKLVSET(pp3)%ENR0(1,IADC(NX1)) = ABS(CRKLVSET(pp1)%ENR0(1,IADC(NX1)))
                CRKLVSET(pp3)%ENR0(1,IADC(NX2)) = CRKLVSET(pp1)%ENR0(1,IADC(NX2))
                CRKLVSET(pp3)%ENR0(1,IADC(NX3)) = CRKLVSET(pp1)%ENR0(1,IADC(NX3))
                CRKLVSET(pp3)%ENR0(1,IADC(NX4)) = CRKLVSET(pp1)%ENR0(1,IADC(NX4))
                CRKLVSET(PP1)%ENR0(1,IADC(NX1)) = -CRKNODIAD(IADC(NX1)) - KNOD2ELC(NX(NX1))*(ILAY-1)
c
              ELSEIF (CRKEDGE(ILAY)%ICUTEDGE(IE2) == 2) THEN
                !  print*,' IMPOSSIBLE CASE'
              ENDIF
            ELSEIF (ITRI == 0) THEN      ! element cut in 2
              XFEM_STR(NXEL)%GBUF%OFF(I) = ZERO                       
              XFEM_STR(NXEL)%BUFLY(ILAY)%LBUF(1,1,1)%OFF(I) = 0  ! 3rd phantom not actif
            ENDIF 
            CRKLVSET(ILEV)%ENR0(2,IADC(1)) = CRKLVSET(ILEV)%ENR0(1,IADC(1))    
            CRKLVSET(ILEV)%ENR0(2,IADC(2)) = CRKLVSET(ILEV)%ENR0(1,IADC(2))    
            CRKLVSET(ILEV)%ENR0(2,IADC(3)) = CRKLVSET(ILEV)%ENR0(1,IADC(3))    
            CRKLVSET(ILEV)%ENR0(2,IADC(4)) = CRKLVSET(ILEV)%ENR0(1,IADC(4))             
C--------------
          ENDIF ! IF(LAYCUT /= 0)
        ENDDO   ! DO I=JFT,JLT
      ENDDO     ! DO I=1,NXLAY
C-----------
      RETURN
      END
