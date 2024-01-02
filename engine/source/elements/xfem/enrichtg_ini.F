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
Chd|  ENRICHTG_INI                  source/elements/xfem/enrichtg_ini.F
Chd|-- called by -----------
Chd|        INIXFEM                       source/elements/xfem/inixfem.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        CLSKEW3                       source/elements/sh3n/coquedk/cdkcoor3.F
Chd|        LSINT4                        source/elements/xfem/crklayer4n_adv.F
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE ENRICHTG_INI(ELBUF_STR,
     .      IXTG     ,NFT      ,JFT     ,JLT    ,NXLAY   ,
     .      IAD_CRKTG,IEL_CRKTG,INOD_CRK,ELCUTC ,NODEDGE ,
     .      CRKNODIAD,KNOD2ELC ,X       ,CRKEDGE,XEDGE3N )
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
#include      "com_xfem1.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXTG(NIXTG,*),NFT,JFT,JLT,INOD_CRK(*),KNOD2ELC(*),
     .        IAD_CRKTG(3,*),IEL_CRKTG(*),ELCUTC(2,*),NODEDGE(2,*),
     .        CRKNODIAD(*),XEDGE3N(3,*),NXLAY
      my_real
     .   X(3,*)
      TYPE (ELBUF_STRUCT_)               :: ELBUF_STR
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,ELCRK,ELCRKTG,ELCUT,pp1,pp2,pp3,IADC(3),IENR0(3),
     . IENR(3),IED,IEDGE,r,IE10,IE20,IE1,IE2,NOD1,NOD2,N(3),DX(6),
     . NX(3),dd(3),LAYCUT,ISIGN1,ISIGN2,ISIGN3,SIGBETA,IAD1,IAD2,
     . ISIGN0(NXEL,3),p1,p2,ICUTEDGE,IBOUNDEDGE,
     . NTAG(3),EDGEENR(3),ENR(3),ILAY,ITRI,NX1,NX2,NX3,NM,NP
      my_real
     .   X1G(MVSIZ),X2G(MVSIZ),X3G(MVSIZ),Y1G(MVSIZ),Y2G(MVSIZ),
     .   Y3G(MVSIZ),Z1G(MVSIZ),Z2G(MVSIZ),Z3G(MVSIZ),AREA(MVSIZ),
     .   LXYZ0(2),RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),
     .   SX(MVSIZ),SY(MVSIZ),SZ(MVSIZ),R11(MVSIZ),R12(MVSIZ),
     .   R13(MVSIZ),R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),R31(MVSIZ),
     .   R32(MVSIZ),R33(MVSIZ),XL1(MVSIZ),YL1(MVSIZ),XL2(MVSIZ),
     .   YL2(MVSIZ),XL3(MVSIZ),YL3(MVSIZ),
     .   FIT(3,MVSIZ),OFFG(MVSIZ),XIN(2,MVSIZ),YIN(2,MVSIZ),XXX,YYY,ZZZ,FI,
     .   XXL(3,MVSIZ),YYL(3,MVSIZ),XN(3),YN(3),XM(2),YM(2)
      my_real X1,Y1,X2,Y2,X3,Y3,X10,Y10,Z10,X20,Y20,Z20,
     .   BETA,AREA1,AREA2,AREA3
      DATA dd/2,3,1/
      DATA DX/1,2,3,1,2,3/
C=======================================================================
C     COORDONNEES (GLOBAL)
C-----------------------
      DO I=JFT,JLT
        X1G(I)=X(1,IXTG(2,I+NFT))
        Y1G(I)=X(2,IXTG(2,I+NFT))
        Z1G(I)=X(3,IXTG(2,I+NFT))
        X2G(I)=X(1,IXTG(3,I+NFT))
        Y2G(I)=X(2,IXTG(3,I+NFT))
        Z2G(I)=X(3,IXTG(3,I+NFT))
        X3G(I)=X(1,IXTG(4,I+NFT))
        Y3G(I)=X(2,IXTG(4,I+NFT))
        Z3G(I)=X(3,IXTG(4,I+NFT))
      ENDDO
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      DO I=JFT,JLT
        RX(I) = X2G(I)-X1G(I)
        RY(I) = Y2G(I)-Y1G(I)
        RZ(I) = Z2G(I)-Z1G(I)
        SX(I) = X3G(I)-X1G(I)
        SY(I) = Y3G(I)-Y1G(I)
        SZ(I) = Z3G(I)-Z1G(I)
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
        XL1(I) = ZERO
        YL1(I) = ZERO
        XL2(I)=R11(I)*RX(I)+R21(I)*RY(I)+R31(I)*RZ(I)
        YL2(I)=R12(I)*RX(I)+R22(I)*RY(I)+R32(I)*RZ(I)
        XL3(I)=R11(I)*SX(I)+R21(I)*SY(I)+R31(I)*SZ(I)
        YL3(I)=R12(I)*SX(I)+R22(I)*SY(I)+R32(I)*SZ(I)
        AREA(I)=HALF*AREA(I)
      ENDDO
C
C     LOCAL TO CENTER OF ELEM (possible warped element)
C
      DO I=JFT,JLT
        LXYZ0(1)=THIRD*(XL1(I)+XL2(I)+XL3(I))
        LXYZ0(2)=THIRD*(YL1(I)+YL2(I)+YL3(I))
C
        XL1(I)=XL1(I)-LXYZ0(1)
        YL1(I)=YL1(I)-LXYZ0(2)
        XL2(I)=XL2(I)-LXYZ0(1)
        YL2(I)=YL2(I)-LXYZ0(2)
        XL3(I)=XL3(I)-LXYZ0(1)
        YL3(I)=YL3(I)-LXYZ0(2)
      ENDDO
c-----------------------------------------------------
c     Loop over layers
c-----------------------------------------------------
      DO ILAY=1,NXLAY
        pp1 = NXEL*(ILAY-1)+1
        pp2 = pp1 + 1
        pp3 = pp2 + 1
C
        DO I=JFT,JLT
          FIT(1,I)=ZERO
          FIT(2,I)=ZERO
          FIT(3,I)=ZERO
        ENDDO
C
        DO I=JFT,JLT
          ELCRKTG = IEL_CRKTG(I+NFT)
          ELCRK   = ELCRKTG + ECRKXFEC
          LAYCUT  = CRKEDGE(ILAY)%LAYCUT(ELCRK)
          IF (LAYCUT /= 0) THEN
            XN(1) = XL1(I)
            YN(1) = YL1(I)
            XN(2) = XL2(I)
            YN(2) = YL2(I)
            XN(3) = XL3(I)
            YN(3) = YL3(I)
            XXL(1,I)=XL1(I)
            YYL(1,I)=YL1(I)
            XXL(2,I)=XL2(I)
            YYL(2,I)=YL2(I)
            XXL(3,I)=XL3(I)
            YYL(3,I)=YL3(I)
            DO K=1,3
              p1 = K
              p2 = dd(K)
              IED = CRKEDGE(ILAY)%IEDGETG(K,ELCRKTG)
              IF (IED > 0) THEN
                IEDGE = XEDGE3N(K,ELCRKTG)
                BETA  = CRKEDGE(ILAY)%RATIO(IEDGE)
                NOD1  = NODEDGE(1,IEDGE)   
                NOD2  = NODEDGE(2,IEDGE)              
                IF (NOD1 == IXTG(K+1,I+NFT) .and. NOD2 == IXTG(dd(K)+1,I+NFT)) THEN
                  p1 = K
                  p2 = dd(K)
                ELSEIF (NOD2 == IXTG(K+1,I+NFT).and.NOD1 == IXTG(dd(K)+1,I+NFT))THEN
                  p1 = dd(K)
                  p2 = K
                ENDIF
                XIN(IED,I) = XN(P1) + BETA*(XN(P2) - XN(P1))
                YIN(IED,I) = YN(P1) + BETA*(YN(P2) - YN(P1)) 
                XM(IED)    = HALF*(XN(p1)+XN(p2))
                YM(IED)    = HALF*(YN(p1)+YN(p2))
              ENDIF
            ENDDO
C
            DO K=1,3
              FI = ZERO
              CALL LSINT4(XM(1),YM(1),XM(2),YM(2),XN(K),YN(K),FI )
              IF (FIT(K,I)==ZERO) FIT(K,I) = FI
            ENDDO
          ENDIF
        ENDDO
C
        IF (ILAY == 1) THEN
          DO I=JFT,JLT
            ELCRKTG = IEL_CRKTG(I+NFT)
            ELCRK   = ELCRKTG + ECRKXFEC
            ELCUT   = CRKEDGE(ILAY)%LAYCUT(ELCRK)
            IF (ELCUT /= 0) THEN
              ELCUTC(1,I+NFT) = 1
              ELCUTC(2,I+NFT) = 1
            ENDIF
          ENDDO
        ENDIF
c--------------------------------------------------------------------
        DO I=JFT,JLT
          ELCRKTG = IEL_CRKTG(I+NFT)
          ELCRK   = ELCRKTG + ECRKXFEC
          LAYCUT  = CRKEDGE(ILAY)%LAYCUT(ELCRK)
          IF (LAYCUT /= 0) THEN
c
            IADC(1) = IAD_CRKTG(1,ELCRKTG)
            IADC(2) = IAD_CRKTG(2,ELCRKTG)
            IADC(3) = IAD_CRKTG(3,ELCRKTG)
C
            IENR0(1) = CRKNODIAD(IADC(1))
            IENR0(2) = CRKNODIAD(IADC(2))
            IENR0(3) = CRKNODIAD(IADC(3))
C
            N(1) = IXTG(2,I+NFT)
            N(2) = IXTG(3,I+NFT)
            N(3) = IXTG(4,I+NFT)
C
            NX(1) = INOD_CRK(N(1))
            NX(2) = INOD_CRK(N(2))
            NX(3) = INOD_CRK(N(3))
C
            IENR(1)  = IENR0(1) + KNOD2ELC(NX(1))*(ILAY-1)
            IENR(2)  = IENR0(2) + KNOD2ELC(NX(2))*(ILAY-1)
            IENR(3)  = IENR0(3) + KNOD2ELC(NX(3))*(ILAY-1)
C
            NTAG(1:3) = 0
            EDGEENR(1:3) = 0
            ENR(1:3) = 0
C
            DO K=1,3
              IED = CRKEDGE(ILAY)%IEDGETG(K,ELCRKTG)
              IF (IED > 0) THEN
                NTAG(K)     = NTAG(K) + 1
                NTAG(dd(K)) = NTAG(dd(K)) + 1
                IEDGE = XEDGE3N(K,ELCRKTG)
                NOD1  = NODEDGE(1,IEDGE)
                NOD2  = NODEDGE(2,IEDGE)
                IE10  = CRKEDGE(ILAY)%EDGEENR(1,IEDGE)
                IE20  = CRKEDGE(ILAY)%EDGEENR(2,IEDGE)
                IF (NOD1 == N(K) .and. NOD2 == N(dd(K))) THEN
                  p1 = K
                  p2 = dd(K)
                ELSEIF (NOD2 == N(K) .and. NOD1 == N(dd(K))) THEN
                  p1 = dd(K)
                  p2 = K
                ENDIF
                EDGEENR(p1) = IE10
                EDGEENR(p2) = IE20
              ENDIF
            ENDDO
C
            DO K=1,3
              IF (NTAG(K) > 0) THEN
                ENR(K) = EDGEENR(K)
              ELSE
                ENR(K) = IENR(K)
              ENDIF
            ENDDO
C
            DO K=1,3
              IF (IENR(K) > IENRNOD) THEN
                WRITE(IOUT,*) 'ERROR CRACK INITIATION,ENRICHMENT NODE EXCEEDED'
                CALL ARRET(2)
              ENDIF
            ENDDO
C
            ISIGN1 = INT(SIGN(ONE,FIT(1,I)))
            ISIGN2 = INT(SIGN(ONE,FIT(2,I)))
            ISIGN3 = INT(SIGN(ONE,FIT(3,I)))
C
            IF (FIT(1,I) == ZERO) ISIGN1 = 0
            IF (FIT(2,I) == ZERO) ISIGN2 = 0
            IF (FIT(3,I) == ZERO) ISIGN3 = 0
C
            DO K=1,NXEL
              ISIGN0(K,1) = ISIGN1
              ISIGN0(K,2) = ISIGN2
              ISIGN0(K,3) = ISIGN3
            ENDDO
c------------------------------------------
            ITRI = 0
            NM   = 0    
            NP   = 0    
            DO K=1,3
              IF (ISIGN0(1,K) > 0) THEN
                ITRI = ITRI + 1
                NP = K
              ELSEIF (ISIGN0(1,K) < 0) THEN
                NM = K
              ENDIF
            ENDDO
            IF (ITRI == 1) THEN
              ITRI = -1
              NX1  = NP          
            ELSEIF (ITRI == 2) THEN
              ITRI = 1
              NX1 = NM 
            ENDIF
            NX2 = DX(NX1+1)                            
            NX3 = DX(NX2+1)                            
            XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK) = ITRI  
            XFEM_PHANTOM(ILAY)%ITRI(2,ELCRK) = NX1     
C
            XFEM_PHANTOM(ILAY)%IFI(IADC(1)) = ISIGN0(1,1)
            XFEM_PHANTOM(ILAY)%IFI(IADC(2)) = ISIGN0(1,2)
            XFEM_PHANTOM(ILAY)%IFI(IADC(3)) = ISIGN0(1,3)
C-------------------
c           IXEL = 1 => positif element within ILAY  (ILEV = PP1)
C-------------------
            CRKLVSET(PP1)%ENR0(1,IADC(1)) = ENR(1)
            CRKLVSET(PP1)%ENR0(1,IADC(2)) = ENR(2)
            CRKLVSET(PP1)%ENR0(1,IADC(3)) = ENR(3)
C
            IF (ISIGN0(1,1) > 0) CRKLVSET(PP1)%ENR0(1,IADC(1)) = 0
            IF (ISIGN0(1,2) > 0) CRKLVSET(PP1)%ENR0(1,IADC(2)) = 0
            IF (ISIGN0(1,3) > 0) CRKLVSET(PP1)%ENR0(1,IADC(3)) = 0
C
            CRKLVSET(PP1)%ENR0(2,IADC(1)) = CRKLVSET(PP1)%ENR0(1,IADC(1))
            CRKLVSET(PP1)%ENR0(2,IADC(2)) = CRKLVSET(PP1)%ENR0(1,IADC(2))
            CRKLVSET(PP1)%ENR0(2,IADC(3)) = CRKLVSET(PP1)%ENR0(1,IADC(3))
C-------------------
c           IXEL = 2 => negatif element within ILAY (ILEV = PP2)
C-------------------
            CRKLVSET(PP2)%ENR0(1,IADC(1)) = ENR(1)
            CRKLVSET(PP2)%ENR0(1,IADC(2)) = ENR(2)
            CRKLVSET(PP2)%ENR0(1,IADC(3)) = ENR(3)
C
            IF (ISIGN0(2,1) < 0) CRKLVSET(PP2)%ENR0(1,IADC(1)) = 0
            IF (ISIGN0(2,2) < 0) CRKLVSET(PP2)%ENR0(1,IADC(2)) = 0
            IF (ISIGN0(2,3) < 0) CRKLVSET(PP2)%ENR0(1,IADC(3)) = 0
C
            CRKLVSET(PP2)%ENR0(2,IADC(1)) = CRKLVSET(PP2)%ENR0(1,IADC(1))
            CRKLVSET(PP2)%ENR0(2,IADC(2)) = CRKLVSET(PP2)%ENR0(1,IADC(2))
            CRKLVSET(PP2)%ENR0(2,IADC(3)) = CRKLVSET(PP2)%ENR0(1,IADC(3))
C-------------------
c           IXEL = 3 => Third phantom element  (ILEV = PP3)
C-------------------
            IF (ITRI < 0) THEN          ! sign ILEV3 = ILEV2 < 0
              IE1 = XEDGE3N(NX1,ELCRKTG) 
              IE2 = XEDGE3N(NX3,ELCRKTG) 
c              IF (CRKEDGE(ILAY)%ICUTEDGE(IE2) == 2) THEN   ! tip edge
              CRKLVSET(PP3)%ENR0(1,IADC(NX1)) = ABS(CRKLVSET(PP2)%ENR0(1,IADC(NX1)))
              CRKLVSET(PP3)%ENR0(1,IADC(NX2)) = CRKLVSET(PP2)%ENR0(1,IADC(NX2))
              CRKLVSET(PP3)%ENR0(1,IADC(NX3)) = CRKLVSET(PP2)%ENR0(1,IADC(NX3))
              CRKLVSET(PP2)%ENR0(1,IADC(NX1)) = -CRKNODIAD(IADC(NX1)) - KNOD2ELC(NX(NX1))*(ILAY-1)
c
              BETA = CRKEDGE(ILAY)%RATIO(IE2)
              NOD1 = NODEDGE(1,IE2)
              NOD2 = NODEDGE(2,IE2)
              K = NX3   ! starting edge node (local) 
              IF (NOD1 == IXTG(K+1,I+NFT) .and.
     .            NOD2 == IXTG(DD(K)+1,I+NFT)) THEN
                P1 = K      
                P2 = DD(K)  
                SIGBETA = IE2
              ELSEIF (NOD2 == IXTG(K+1,I+NFT) .and.
     .                NOD1 == IXTG(DD(K)+1,I+NFT)) THEN
                P1 = DD(K)  
                P2 = K      
                SIGBETA = -IE2
              ENDIF
c
c--           AREA factors
              X1  = XXL(NX1,I)    
              Y1  = YYL(NX1,I)                   
              IED = CRKEDGE(ILAY)%IEDGETG(NX1,ELCRKTG)         
              X2  = XIN(IED,I)                            
              Y2  = YIN(IED,I)                            
              IED = CRKEDGE(ILAY)%IEDGETG(NX3,ELCRKTG)         
              X3  = XIN(IED,I)                            
              Y3  = YIN(IED,I)                            
              AREA1 = HALF*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))
              AREA1 = AREA1 / AREA(I)
              X1  = XXL(NX2,I)    
              Y1  = YYL(NX2,I)                   
              X2  = XXL(NX3,I)    
              Y2  = YYL(NX3,I)                   
              AREA2 = HALF*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))
              AREA3 = ONE - AREA1 - AREA2
c
            ELSEIF (ITRI > 0) THEN      ! sign ILEV3 = ILEV1 > 0
c
              IE1 = XEDGE3N(NX1,ELCRKTG)   ! tip edge
c              IF (CRKEDGE(ILAY)%ICUTEDGE(IE1) == 2) THEN
              CRKLVSET(PP3)%ENR0(1,IADC(NX1)) = ABS(CRKLVSET(PP1)%ENR0(1,IADC(NX1)))
              CRKLVSET(PP3)%ENR0(1,IADC(NX2)) = CRKLVSET(PP1)%ENR0(1,IADC(NX2))
              CRKLVSET(PP3)%ENR0(1,IADC(NX3)) = CRKLVSET(PP1)%ENR0(1,IADC(NX3))
              CRKLVSET(PP1)%ENR0(1,IADC(NX1)) = -CRKNODIAD(IADC(NX1)) - KNOD2ELC(NX(NX1))*(ILAY-1)
c
              BETA = CRKEDGE(ILAY)%RATIO(IE1)
              NOD1 = NODEDGE(1,IE1)
              NOD2 = NODEDGE(2,IE1)
              K = NX1   ! starting edge node (local) 
              IF (NOD1 == IXTG(K+1,I+NFT) .and.
     .            NOD2 == IXTG(DD(K)+1,I+NFT)) THEN
                P1 = K      
                P2 = DD(K)  
                SIGBETA = IE1
              ELSEIF (NOD2 == IXTG(K+1,I+NFT) .and.
     .                NOD1 == IXTG(DD(K)+1,I+NFT)) THEN
                P1 = DD(K)  
                P2 = K      
                SIGBETA = -IE1
              ENDIF
c
c--           AREA factors for each phantom                          
              X1  = XXL(NX1,I)                                       
              Y1  = YYL(NX1,I)                                       
              IED = CRKEDGE(ILAY)%IEDGETG(NX3,ELCRKTG)               
              X2  = XIN(IED,I)                                       
              Y2  = YIN(IED,I)                                       
              IED = CRKEDGE(ILAY)%IEDGETG(NX1,ELCRKTG)               
              X3  = XIN(IED,I)                                       
              Y3  = YIN(IED,I)                                       
              AREA1 = HALF*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))  
              AREA1 = AREA1 / AREA(I)                                
              X1  = XXL(NX2,I)                                       
              Y1  = YYL(NX2,I)                                       
              X3  = XXL(NX3,I)                                       
              Y3  = YYL(NX3,I)                                       
              AREA2 = HALF*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))  
              AREA3 = ONE - AREA1 - AREA2                             
c
            ENDIF  ! ITRI
c
            CRKLVSET(PP3)%ENR0(2,IADC(1)) = CRKLVSET(PP3)%ENR0(1,IADC(1))  
            CRKLVSET(PP3)%ENR0(2,IADC(2)) = CRKLVSET(PP3)%ENR0(1,IADC(2))  
            CRKLVSET(PP3)%ENR0(2,IADC(3)) = CRKLVSET(PP3)%ENR0(1,IADC(3))  
c
            CRKLVSET(PP1)%AREA(ELCRK) = AREA1                                                       
            CRKLVSET(PP2)%AREA(ELCRK) = AREA2                                                       
            CRKLVSET(PP3)%AREA(ELCRK) = AREA3                                                      
C-------------------
          ENDIF  ! LAYCUT /= 0
        ENDDO    ! DO I=JFT,JLT
      ENDDO      ! DO ILAY=1,NXLAY
C-------------
      RETURN
      END
