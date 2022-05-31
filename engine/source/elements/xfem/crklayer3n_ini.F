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
Chd|  CRKLAYER3N_INI                source/elements/xfem/crklayer3n_ini.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        LSINT4                        source/elements/xfem/crklayer4n_adv.F
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CRKLAYER3N_INI(
     .             NEL       ,NFT       ,ILAY      ,NLAY      ,IXTG      ,
     .             ELCUTC    ,ELCRKINI  ,IEL_CRKTG ,INOD_CRK  ,IAD_CRKTG ,      
     .             NODENR    ,DIR1      ,DIR2      ,NODEDGE   ,CRKNODIAD ,
     .             KNOD2ELC  ,CRKEDGE   ,XEDGE3N   ,NGL       ,AREA      ,
     .             XL2       ,XL3       ,YL2       ,YL3       )
C-----------------------------------------------
C crack initialisation, shells 3N
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "com_xfem1.inc"
#include      "comlock.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NFT,ILAY,NLAY
      INTEGER IXTG(NIXTG,*),IEL_CRKTG(*),NGL(NEL),
     . INOD_CRK(*),NODENR(*),IAD_CRKTG(3,*),ELCRKINI(NLAY,*),
     . ELCUTC(2,*),NODEDGE(2,*),CRKNODIAD(*),KNOD2ELC(*),XEDGE3N(3,*)
      my_real, DIMENSION(NEL) :: AREA,XL2,YL2,XL3,YL3
      my_real, DIMENSION(NLAY,NEL) :: DIR1,DIR2
      TYPE (XFEM_EDGE_)   , DIMENSION(*) :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,ILEV,IR,P1,P2,IED,IED1,IED2,PP1,PP2,PP3,FAC,
     .  ICRK,LAYCUT,NEWCRK,ELCRK,ELCRKTG,IEDGE,ICUT,SIGBETA,ITRI,NM,NP,
     .  NX1,NX2,NX3,NOD1,NOD2,IE1,IE2,IFI1,IFI2,IE10,IE20,IAD1,IAD2,IAD3
      INTEGER JCT(NEL),EDGEL(3,NEL),IADC(3),ISIGN(3),
     .  DD(3),D(6),DX(6),INV(2),N(3),NN(3),IENR0(3),IENR(3),IFI(2)
C----------
      my_real, DIMENSION(NEL)   ::  XL1,YL1
      my_real, DIMENSION(2,NEL) ::  XIN,YIN
      my_real, DIMENSION(3,NEL) ::  XXL,YYL,LEN,FIT
      my_real BETA0(3,NEL),XN(3),YN(3),ZN(3),XMI(2),YMI(2)
      my_real BETA,XINT,YINT,FI,BMIN,BMAX,M12,MM,CROSS1,CROSS12,
     .   DIR11,DIR22,X1,Y1,X2,Y2,X3,Y3,AREA1,AREA2,AREA3
c------------------------
      DATA d/1,2,2,3,1,3/
      DATA dd/2,3,1/
      DATA DX/1,2,3,1,2,3/
      PARAMETER (BMIN = 0.01, BMAX = 0.99)
C=======================================================================
      NEWCRK = 0
      DO I=1,NEL
        JCT(I) = 0
        IF (ELCRKINI(ILAY,I) == -1) THEN
          NEWCRK = NEWCRK + 1
          JCT(NEWCRK) = I
        ENDIF
      ENDDO
      IF (NEWCRK == 0) RETURN
C-----------------------
      PP1 = NXEL*(ILAY-1) + 1  
      PP2 = PP1 + 1            
      PP3 = PP1 + 2            
c
      DO I=1,NEL
        BETA0(1:3,I) = ZERO
C
        EDGEL(1,I)=0
        EDGEL(2,I)=0
        EDGEL(3,I)=0
C
        XIN(1,I) = ZERO  ! first inters point in local skew
        YIN(1,I) = ZERO
        XIN(2,I) = ZERO  ! second inters point in local skew
        YIN(2,I) = ZERO
      ENDDO
C----------------------------------------------------------------------
c     local coords
C----------------------------------------------------------------------
      DO I=1,NEL
        XL1(I)   = ZERO
        YL1(I)   = ZERO
        XXL(1,I) = XL1(I)
        YYL(1,I) = YL1(I)
        XXL(2,I) = XL2(I)
        YYL(2,I) = YL2(I)
        XXL(3,I) = XL3(I)
        YYL(3,I) = YL3(I)
      ENDDO
c
      DO I=1,NEL
        LEN(1,I) = (XL2(I)-XL1(I))*(XL2(I)-XL1(I))
     .           + (YL2(I)-YL1(I))*(YL2(I)-YL1(I))
        LEN(2,I) = (XL3(I)-XL2(I))*(XL3(I)-XL2(I))
     .           + (YL3(I)-YL2(I))*(YL3(I)-YL2(I))
        LEN(3,I) = (XL1(I)-XL3(I))*(XL1(I)-XL3(I))
     .           + (YL1(I)-YL3(I))*(YL1(I)-YL3(I))
      ENDDO
C------------------------------------------------
C - intersections -
C------------------------------------------------
      DO IR=1,NEWCRK
        I=JCT(IR)
        FAC = 0
C---
        DIR11 = -DIR2(ILAY,I)
        DIR22 =  DIR1(ILAY,I)
C---
        IF (DIR11 == ZERO) THEN
          DO 140 K=1,3
            XINT = ZERO
            YINT = ZERO
            ELCRKTG = IEL_CRKTG(I+NFT)
            ELCRK = ELCRKTG + ECRKXFEC
            IEDGE = XEDGE3N(K,ELCRKTG)
            NOD1 = NODEDGE(1,IEDGE)
            NOD2 = NODEDGE(2,IEDGE)
            IF (NOD1 == IXTG(K+1,I) .and. NOD2 == IXTG(dd(K)+1,I))THEN
              p1 = K
              p2 = dd(K)
            ELSEIF (NOD2 == IXTG(K+1,I).and.NOD1 == IXTG(dd(K)+1,I))THEN
              p1 = dd(K)
              p2 = K
            ENDIF
C
            IF (XXL(p1,I) == XXL(p2,I)) GOTO 140 ! no inters (parallel to dir 2)
            M12 =  XXL(p2,I)-XXL(p1,I)
            M12 = (YYL(p2,I)-YYL(p1,I))/M12
            XINT = ZERO
            YINT = YYL(p1,I) - M12*XXL(p1,I)
            CROSS12 = (XINT-XXL(p1,I))*(XINT-XXL(p2,I))+
     .                (YINT-YYL(p1,I))*(YINT-YYL(p2,I))
C
            IF(CROSS12 > ZERO)GOTO 140
            FAC = FAC + 1
            CROSS1 = (XXL(p1,I) - XINT)**2 + (YYL(p1,I) - YINT)**2
            BETA   = SQRT(CROSS1 / LEN(K,I))
            BETA   = MAX(BETA, BMIN)
            BETA   = MIN(BETA, BMAX)
            BETA0(K,I) = BETA
C---
            XIN(FAC,I) = XINT
            YIN(FAC,I) = YINT
            EDGEL(K,I) = FAC
            IF(FAC == 2)EXIT
C---
 140    CONTINUE
C---
        ELSEIF(DIR22 == ZERO)THEN
          DO 150 K=1,3
            XINT = ZERO
            YINT = ZERO
            ELCRKTG = IEL_CRKTG(I+NFT)
            ELCRK = ELCRKTG + ECRKXFEC
            IEDGE = XEDGE3N(K,ELCRKTG)
            NOD1 = NODEDGE(1,IEDGE)
            NOD2 = NODEDGE(2,IEDGE)
            IF (NOD1 == IXTG(K+1,I) .and. NOD2 == IXTG(dd(K)+1,I))THEN
              p1 = K
              p2 = dd(K)
            ELSEIF (NOD2 == IXTG(K+1,I).and.NOD1 == IXTG(dd(K)+1,I))THEN
              p1 = dd(K)
              p2 = K
            ENDIF
C
            IF (YYL(p1,I) == YYL(p2,I)) GOTO 150 ! no inters (parallel to dir 2)
            M12 =  YYL(p2,I)-YYL(p1,I)
            M12 = (XXL(p2,I)-XXL(p1,I))/M12
            YINT = ZERO
            XINT = XXL(p1,I) - M12*YYL(p1,I)
            CROSS12 = (XINT-XXL(p1,I))*(XINT-XXL(p2,I))+
     .                (YINT-YYL(p1,I))*(YINT-YYL(p2,I))
C
            IF (CROSS12 > ZERO) GOTO 150
            FAC = FAC + 1
C
            CROSS1 = (XXL(p1,I) - XINT)**2 + (YYL(p1,I) - YINT)**2
            BETA   = SQRT(CROSS1 / LEN(K,I))
            BETA   = MAX(BETA, BMIN)
            BETA   = MIN(BETA, BMAX)
            BETA0(K,I) = BETA
C---
            XIN(FAC,I) = XINT
            YIN(FAC,I) = YINT
            EDGEL(K,I) = FAC
            IF(FAC == 2)EXIT
C---
 150    CONTINUE
C---
        ELSEIF (DIR11 /= ZERO .AND. DIR22 /= ZERO)THEN
          DO 160 K=1,3
            XINT = ZERO
            YINT = ZERO
            ELCRKTG = IEL_CRKTG(I+NFT)
            ELCRK = ELCRKTG + ECRKXFEC
            IEDGE = XEDGE3N(K,ELCRKTG)
            NOD1 = NODEDGE(1,IEDGE)
            NOD2 = NODEDGE(2,IEDGE)
            IF (NOD1 == IXTG(K+1,I) .and. NOD2 == IXTG(dd(K)+1,I))THEN
              p1 = K
              p2 = dd(K)
            ELSEIF (NOD2 == IXTG(K+1,I).and.NOD1 == IXTG(dd(K)+1,I))THEN
              p1 = dd(K)
              p2 = K
            ENDIF
C
            IF (XXL(p1,I) == XXL(p2,I)) THEN
              MM = DIR22/DIR11  ! slope of dir (2)
              XINT = XXL(p1,I)   !  or = XXL(p2,I)
              YINT = MM*XINT
              CROSS12 = (XINT-XXL(p1,I))*(XINT-XXL(p2,I))+
     .                  (YINT-YYL(p1,I))*(YINT-YYL(p2,I))
C
              IF (CROSS12 > ZERO) GOTO 160
              FAC = FAC + 1
C
              CROSS1 = (XXL(p1,I) - XINT)**2 + (YYL(p1,I) - YINT)**2
              BETA   = SQRT(CROSS1 / LEN(K,I))
              BETA   = MAX(BETA, BMIN)
              BETA   = MIN(BETA, BMAX)
              BETA0(K,I) = BETA
C---
              XIN(FAC,I) = XINT
              YIN(FAC,I) = YINT
              EDGEL(K,I) = FAC
              IF(FAC == 2)EXIT
            ELSE
              MM = DIR22/DIR11  ! slope of dir (2)
              M12 =  XXL(p2,I)-XXL(p1,I)
              M12 = (YYL(p2,I)-YYL(p1,I))/M12
              IF (MM == M12) GOTO 160  ! no inters (parallel to dir 2)
              XINT = (YYL(p1,I) - M12*XXL(p1,I))/(MM - M12)
              YINT = MM*XINT
              CROSS12 = (XINT-XXL(p1,I))*(XINT-XXL(p2,I))+
     .                  (YINT-YYL(p1,I))*(YINT-YYL(p2,I))
C
              IF (CROSS12 > ZERO) GOTO 160
              FAC = FAC + 1
C
              CROSS1 = (XXL(p1,I) - XINT)**2 + (YYL(p1,I) - YINT)**2
              BETA   = SQRT(CROSS1 / LEN(K,I))
              BETA   = MAX(BETA, BMIN)
              BETA   = MIN(BETA, BMAX)
              BETA0(K,I) = BETA
C---
              XIN(FAC,I) = XINT
              YIN(FAC,I) = YINT
              EDGEL(K,I) = FAC
              IF(FAC == 2)EXIT
            ENDIF
C---
 160    CONTINUE
C---
        ENDIF
      ENDDO
C
C check for getting both intersections
C
      DO IR=1,NEWCRK
        I=JCT(IR)
        FAC = 0
        DO K=1,3
           IF(EDGEL(K,I).EQ.1.or.EDGEL(K,I).EQ.2) FAC=FAC+1
        ENDDO
        IF(FAC /= 2)THEN
          WRITE(IOUT,*) 'ERROR IN INITIATION CRACK.NO CUT EDGES'
          CALL ARRET(2)           
        ENDIF
      ENDDO
C---
      DO IR=1,NEWCRK
        I=JCT(IR)
        ELCRKTG = IEL_CRKTG(I+NFT)
        ELCRK = ELCRKTG + ECRKXFEC
C---
cc        ICRK =  ICRK + 1 ! increase nb of cracks
C---
        DO K=1,3
          IED = EDGEL(K,I)
          IF (IED == 1 .or. IED == 2) THEN
            CRKEDGE(ILAY)%IEDGETG(K,ELCRKTG) = IED
          ENDIF
        ENDDO
      ENDDO
C----------------------------------------------------------------------
C     SIGN DISTANCE FOR NEW CRACKED LAYER
C----------------------------------------------------------------------
      DO IR=1,NEWCRK
        I=JCT(IR)
        FIT(1,I)= ZERO
        FIT(2,I)= ZERO
        FIT(3,I)= ZERO
        XN(1) = XL1(I)
        YN(1) = YL1(I)
        XN(2) = XL2(I)
        YN(2) = YL2(I)
        XN(3) = XL3(I)
        YN(3) = YL3(I)
        DO K=1,3
          p1 = K
          p2 = dd(K)
          IED = EDGEL(K,I)
          IF (IED > 0) THEN
            XMI(IED) = HALF*(XN(p1)+XN(p2))
            YMI(IED) = HALF*(YN(p1)+YN(p2))
          ENDIF
        ENDDO
C
        DO K=1,3
          FI=ZERO
          CALL LSINT4(XMI(1),YMI(1),XMI(2),YMI(2),XN(K),YN(K),FI)
          IF (FIT(K,I)==ZERO) FIT(K,I) = FI
        ENDDO
      ENDDO
C-----------------------
      DO IR=1,NEWCRK
        I=JCT(IR)
        ELCRKTG = IEL_CRKTG(I+NFT)
        ELCRK   = ELCRKTG + ECRKXFEC
C
        DO K=1,3
          IED = EDGEL(K,I)
          IF (IED == 1 .or. IED == 2) THEN
            IEDGE = XEDGE3N(K,ELCRKTG)

            ICUT = CRKEDGE(ILAY)%ICUTEDGE(IEDGE)
            IF (ICUT > 0) THEN   ! already cut before => edge connecting two cracks
              CRKEDGE(ILAY)%ICUTEDGE(IEDGE) = 3   ! only for spmd si 2 cracks sur le meme edge
            ELSE 
              CRKEDGE(ILAY)%ICUTEDGE(IEDGE) = 2  ! edge cut             
              CRKEDGE(ILAY)%RATIO(IEDGE) = BETA0(K,I)
            ENDIF 
          ENDIF
        ENDDO
      ENDDO
C-----------------------
c     FILL new cut phantom element
C-----------------------
      DO IR=1,NEWCRK
        I = JCT(IR)
        ELCRKTG = IEL_CRKTG(I+NFT)
        ELCRK = ELCRKTG + ECRKXFEC
C
        IADC(1) = IAD_CRKTG(1,ELCRKTG)
        IADC(2) = IAD_CRKTG(2,ELCRKTG)
        IADC(3) = IAD_CRKTG(3,ELCRKTG)
C
        N(1) = IXTG(2,I)
        N(2) = IXTG(3,I)
        N(3) = IXTG(4,I)
C
        NN(1) = INOD_CRK(N(1))
        NN(2) = INOD_CRK(N(2))
        NN(3) = INOD_CRK(N(3))
C
        ELCUTC(1,I) = 2
        NUMELCRK    = NUMELCRK + 1
C
        ISIGN(1) = INT(SIGN(ONE,FIT(1,I)))
        ISIGN(2) = INT(SIGN(ONE,FIT(2,I)))
        ISIGN(3) = INT(SIGN(ONE,FIT(3,I)))
C
        IF (FIT(1,I) == ZERO) ISIGN(1) = 0
        IF (FIT(2,I) == ZERO) ISIGN(2) = 0
        IF (FIT(3,I) == ZERO) ISIGN(3) = 0
c--------------------------------------------
c
        ICRK = CRKSHELL(PP1)%CRKSHELLID(ELCRK)
        ITRI = 0                                                                        
        NM   = 0                                                                          
        NP   = 0                                                                          
        DO K=1,3                                                                          
          IF (ISIGN(K) > 0) THEN                                                       
            ITRI = ITRI + 1                                                           
            NP = K                                                                        
          ELSEIF (ISIGN(K) < 0) THEN                                                   
            NM = K                                                                        
          ENDIF                                                                           
        ENDDO                                                                             
        IF (ITRI == 1) THEN                                                             
          ITRI = -1
          NX1  = NP                                                                        
        ELSEIF (ITRI == 2) THEN                                                         
          ITRI = 1                                                                      
          NX1  = NM
        ENDIF                                                                             
        NX2 = DX(NX1+1)                                                                   
        NX3 = DX(NX2+1)                                                                   
        XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK) = ITRI                                         
        XFEM_PHANTOM(ILAY)%ITRI(2,ELCRK) = NX1                                            
c--------------------------------------------
c       activate enriched nodes
c--------------------------------------------
c
        IENR0(1) = CRKNODIAD(IADC(1))
        IENR0(2) = CRKNODIAD(IADC(2))
        IENR0(3) = CRKNODIAD(IADC(3))
C
        IENR(1)  = IENR0(1) + KNOD2ELC(NN(1))*(ILAY-1)
        IENR(2)  = IENR0(2) + KNOD2ELC(NN(2))*(ILAY-1)
        IENR(3)  = IENR0(3) + KNOD2ELC(NN(3))*(ILAY-1)
C
        SIGBETA = 0
        DO K=1,3
          IED = EDGEL(K,I)
          IF (IED > 0) THEN
            IEDGE = XEDGE3N(K,ELCRKTG)
            NOD1 = NODEDGE(1,IEDGE)
            NOD2 = NODEDGE(2,IEDGE)
            IE10 = 0
            IE20 = 0
            IE10 = CRKEDGE(ILAY)%EDGEENR(1,IEDGE)
            IE20 = CRKEDGE(ILAY)%EDGEENR(2,IEDGE)
            IF (NOD1 == N(K) .and. NOD2 == N(dd(K))) THEN
              IE1 = IENR(K)
              IE2 = IENR(dd(K))
              IFI1 = ISIGN(K)
              IFI2 = ISIGN(dd(K))
              SIGBETA = IEDGE
            ELSE IF (NOD2 == N(K) .and. NOD1 == N(dd(K))) THEN
              IE1 = IENR(dd(K))
              IE2 = IENR(K)
              IFI1 = ISIGN(dd(K))
              IFI2 = ISIGN(K)
              SIGBETA = -IEDGE
            END IF
            CRKEDGE(ILAY)%EDGEENR(1,IEDGE) = MAX(IE1,IE10)
            CRKEDGE(ILAY)%EDGEENR(2,IEDGE) = MAX(IE2,IE20)
            IF (CRKEDGE(ILAY)%EDGEICRK(IEDGE) == 0)
     .          CRKEDGE(ILAY)%EDGEICRK(IEDGE) = ICRK
          ENDIF
        ENDDO
C------------------
        CRKEDGE(ILAY)%LAYCUT(ELCRK) = -1 ! layer cut
        XFEM_PHANTOM(ILAY)%ELCUT(ELCRK) = ICRK
C------------------
        DO K=1,3
          IED = EDGEL(K,I)
          IEDGE = XEDGE3N(K,ELCRKTG)
          IF (IED > 0) THEN
            CRKEDGE(ILAY)%EDGETIP(1,IEDGE) = IED
            CRKEDGE(ILAY)%EDGETIP(2,IEDGE) = 
     .                  CRKEDGE(ILAY)%EDGETIP(2,IEDGE) + 1
          ENDIF
        ENDDO
C
        XFEM_PHANTOM(ILAY)%IFI(IADC(1)) = ISIGN(1)
        XFEM_PHANTOM(ILAY)%IFI(IADC(2)) = ISIGN(2)
        XFEM_PHANTOM(ILAY)%IFI(IADC(3)) = ISIGN(3)
C------------------
c       IXEL = 1 => positif element within ILAY  (ILEV = PP1)
C------------------
        ILEV = pp1
C
        CRKLVSET(ILEV)%ENR0(1,IADC(1)) = -IENR(1)
        CRKLVSET(ILEV)%ENR0(1,IADC(2)) = -IENR(2)
        CRKLVSET(ILEV)%ENR0(1,IADC(3)) = -IENR(3)
C
        IF (ISIGN(1) > 0) CRKLVSET(ILEV)%ENR0(1,IADC(1)) = 0
        IF (ISIGN(2) > 0) CRKLVSET(ILEV)%ENR0(1,IADC(2)) = 0
        IF (ISIGN(3) > 0) CRKLVSET(ILEV)%ENR0(1,IADC(3)) = 0
C------------------
c       IXEL = 2 => negatif element within ILAY (ILEV = PP2)
C------------------
        ILEV = pp2
C------------------
        CRKLVSET(ILEV)%ENR0(1,IADC(1)) = -IENR(1)
        CRKLVSET(ILEV)%ENR0(1,IADC(2)) = -IENR(2)
        CRKLVSET(ILEV)%ENR0(1,IADC(3)) = -IENR(3)
C
        IF (ISIGN(1) < 0) CRKLVSET(ILEV)%ENR0(1,IADC(1)) = 0
        IF (ISIGN(2) < 0) CRKLVSET(ILEV)%ENR0(1,IADC(2)) = 0
        IF (ISIGN(3) < 0) CRKLVSET(ILEV)%ENR0(1,IADC(3)) = 0
C------------------
c       IXEL = 3 => Third phantom element  (ILEV = PP3)
C------------------
        IF (ITRI < 0) THEN          ! sign ILEV3 = ILEV2 < 0                            
          IE2 = XEDGE3N(NX3,ELCRKTG)   ! tip edge                                                   
          IF (CRKEDGE(ILAY)%ICUTEDGE(IE2) > 1) THEN                                               
            SIGBETA = -SIGBETA
            IAD1 = IADC(NX1)    
            IAD2 = IADC(NX2)  
            IAD3 = IADC(NX3)
            CRKLVSET(PP3)%ENR0(1,IAD1) = ABS(CRKLVSET(PP2)%ENR0(1,IAD1))                  
            CRKLVSET(PP3)%ENR0(1,IAD2) = CRKLVSET(PP2)%ENR0(1,IAD2)                       
            CRKLVSET(PP3)%ENR0(1,IAD3) = CRKLVSET(PP2)%ENR0(1,IAD3)                       
            CRKLVSET(PP2)%ENR0(1,IAD1) = -CRKNODIAD(IAD1) - KNOD2ELC(NN(NX1))*(ILAY-1)    
c
c--         AREA factors for each phantom
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
            AREA2 = AREA2 / AREA(I)
            AREA3 = ONE - AREA1 - AREA2
c
          ELSE                                                                                      
            ! print*,' IMPOSSIBLE CASE'                                                               
          ENDIF                                                                                     
c
        ELSEIF (ITRI > 0) THEN      ! sign ILEV3 = ILEV1 > 0                            
c
          IE1 = XEDGE3N(NX1,ELCRKTG)   ! tip edge                                                   
          IF (CRKEDGE(ILAY)%ICUTEDGE(IE1) > 1) THEN                                               
            IAD1 = IADC(NX1)    
            IAD2 = IADC(NX2)  
            IAD3 = IADC(NX3)
            CRKLVSET(PP3)%ENR0(1,IAD1) = ABS(CRKLVSET(PP1)%ENR0(1,IAD1))                  
            CRKLVSET(PP3)%ENR0(1,IAD2) = CRKLVSET(PP1)%ENR0(1,IAD2)                       
            CRKLVSET(PP3)%ENR0(1,IAD3) = CRKLVSET(PP1)%ENR0(1,IAD3)                       
            CRKLVSET(PP1)%ENR0(1,IAD1) = -CRKNODIAD(IAD1) - KNOD2ELC(NN(NX1))*(ILAY-1)    
c
c--         AREA factors for each phantom
            IED1= CRKEDGE(ILAY)%IEDGETG(NX1,ELCRKTG)         
            IED2= CRKEDGE(ILAY)%IEDGETG(NX3,ELCRKTG)         
            X1  = XIN(IED1,I) 
            Y1  = YIN(IED1,I)                
            X2  = XXL(NX2,I) 
            Y2  = YYL(NX2,I)                
            X3  = XXL(NX3,I)                           
            Y3  = YYL(NX3,I)                           
            AREA1 = HALF*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))
            AREA1 = AREA1 / AREA(I)
            X1  = XXL(NX1,I) 
            Y1  = YYL(NX1,I)                
            X2  = XIN(IED1,I)                            
            Y2  = YIN(IED1,I)                            
            X3  = XIN(IED2,I)                            
            Y3  = YIN(IED2,I)                            
            AREA2 = HALF*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))
            AREA2 = AREA2 / AREA(I)
            AREA3 = ONE - AREA1 - AREA2
          ELSE                                                                                      
            ! print*,' IMPOSSIBLE CASE'                                                               
          ENDIF                                                                                     
        ENDIF    ! ITRI                                                                                    
c
        CRKLVSET(PP1)%AREA(ELCRK) = AREA1                                                           
        CRKLVSET(PP2)%AREA(ELCRK) = AREA2                                                           
        CRKLVSET(PP3)%AREA(ELCRK) = AREA3                                                          
c
      ENDDO !  IR=1,NEWCRK
C-------------------
      NLEVSET = NLEVSET + 1  ! update nb of cracks
C-------------------
      RETURN
      END
