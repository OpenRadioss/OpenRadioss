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
Chd|  CRKLAYER4N_ADV                source/elements/xfem/crklayer4n_adv.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        LSINT4                        source/elements/xfem/crklayer4n_adv.F
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|====================================================================
      SUBROUTINE CRKLAYER4N_ADV(
     .           XFEM_STR  ,NEL       ,NFT       ,IXC       ,ELCUTC    ,
     .           ILAY      ,NLAY      ,IEL_CRK   ,INOD_CRK  ,
     .           IADC_CRK  ,NODENR    ,ELCRKINI  ,DIR1      ,DIR2      ,
     .           NODEDGE   ,CRKNODIAD ,KNOD2ELC  ,CRKEDGE   ,A_I       ,
     .           XL2       ,XL3       ,XL4       ,YL2       ,YL3       ,
     .           YL4       ,XEDGE4N   ,NGL       )
C-----------------------------------------------
C   crack advancement, shells 4N
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE ELBUFDEF_MOD
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
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NFT,IXC(NIXC,*),ILAY,NLAY,NGL(NEL),IEL_CRK(*),
     . INOD_CRK(*),NODENR(*),IADC_CRK(4,*),ELCRKINI(NLAY,NEL),
     . ELCUTC(2,*),NODEDGE(2,*),CRKNODIAD(*),KNOD2ELC(*),XEDGE4N(4,*)
      my_real DIR1(NLAY,NEL),DIR2(NLAY,NEL),A_I(NEL),
     .  XL2(NEL),YL2(NEL),XL3(NEL),YL3(NEL),XL4(NEL),YL4(NEL)
      TYPE (ELBUF_STRUCT_), TARGET          :: ELBUF_STR
      TYPE (ELBUF_STRUCT_), DIMENSION(NXEL) :: XFEM_STR
      TYPE (XFEM_EDGE_)   , DIMENSION(*)    :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,II,R,IR,P1,P2,PP1,PP2,PP3,NEWCRK,IED,OK,
     . ICRK,IELCRK,ELCRK,NX1,NX2,NX3,NX4,NM,NP,FAC,IFI1,IFI2,IEDGE,
     . ICUT,SIGBETA,NOD1,NOD2,IE1,IE2,IE10,IE20,ITRI,IAD1,IAD2,IAD3,IAD4
c
      INTEGER ECUT(2,NEL),EDGEL(4,NEL),IENR0(4,NEL),dd(4),d(8),KPERM(8),
     . N(4),NN(4),IADC(4),IENR(4),ENR(4),ISIGN(4),EN(2,4)
      INTEGER , DIMENSION(NEL) :: JCT,ELFISS,TIP
c
      my_real, DIMENSION(2) :: XMI,YMI
      my_real, DIMENSION(2,NEL) :: XIN,YIN
      my_real, DIMENSION(4,NEL) :: LEN,XXL,YYL,FIT,BETA0
      my_real
     .   XINT,YINT,FI,CROSS,ACD,BCD,DLX,DLY,D12,M12,MM,XINT0,YINT0,BETA,
     .   BMIN,BMAX,DIR11,DIR22,X1,Y1,X2,Y2,X3,Y3,X4,Y4,AREA1,AREA2,AREA3
C----------
      DATA d/1,2,2,3,4,3,1,4/
      DATA dd/2,3,4,1/
      DATA KPERM/1,2,3,4,1,2,3,4/
      PARAMETER (BMIN = 0.01, BMAX = 0.99)
C=======================================================================
      NEWCRK = 0
      DO I=1,NEL
        JCT(I) = 0
        IF (ELCRKINI(ILAY,I) == 1) THEN  ! avancement de fissure
          NEWCRK = NEWCRK + 1
          JCT(NEWCRK) = I
        ENDIF
      ENDDO
      IF (NEWCRK == 0) RETURN
C---
      II  = NXEL*(ILAY-1)
      PP1 = II + 1
      PP2 = II + 2
      PP3 = II + 3
C---
      DO IR=1,NEWCRK
        I = JCT(IR)
        ELFISS(I)= 0
        TIP(I)   = 0
        ECUT(1:2,I)  = 0
        EDGEL(1:4,I) = 0
        IENR0(1:4,I) = 0
        BETA0(1:4,I) = ZERO
        XIN(1,I) = ZERO  ! first inters point in local skew
        YIN(1,I) = ZERO
        XIN(2,I) = ZERO  ! second inters point in local skew
        YIN(2,I) = ZERO
c
        XXL(1,I) = ZERO
        YYL(1,I) = ZERO
        XXL(2,I) = XL2(I)
        YYL(2,I) = YL2(I)
        XXL(3,I) = XL3(I)
        YYL(3,I) = YL3(I)
        XXL(4,I) = XL4(I)
        YYL(4,I) = YL4(I)
c
        LEN(1,I) = XL2(I)*XL2(I) + YL2(I)*YL2(I)
        LEN(2,I) = (XL3(I)-XL2(I))*(XL3(I)-XL2(I))+
     .             (YL3(I)-YL2(I))*(YL3(I)-YL2(I))
        LEN(3,I) = (XL4(I)-XL3(I))*(XL4(I)-XL3(I))+
     .             (YL4(I)-YL3(I))*(YL4(I)-YL3(I))
        LEN(4,I) = XL4(I)*XL4(I) + YL4(I)*YL4(I)
      ENDDO
C------------------------------------------------
c     First intersection (already cut edge)
C------------------------------------------------
      DO IR=1,NEWCRK ! loop over elems (layers) with advancing cracks
        I = JCT(IR)
        ELCRK = IEL_CRK(I+NFT)   ! N  element sys xfem
        OK    = 0
        ICUT  = 0
        IED   = 0
        DO K=1,4  ! edges
          IEDGE = XEDGE4N(K,ELCRK)
          IF (IEDGE > 0) THEN
            ICUT  = CRKEDGE(ILAY)%ICUTEDGE(IEDGE)
            IF (ICUT == 1) THEN
              NOD1  = NODEDGE(1,IEDGE)  ! noeud std
              NOD2  = NODEDGE(2,IEDGE)
              IF (NOD1 == IXC(K+1,I) .and. NOD2 == IXC(dd(K)+1,I)) THEN
                p1 = K
                p2 = dd(K)
              ELSE IF (NOD2 == IXC(K+1,I) .and. NOD1 == IXC(dd(K)+1,I)) THEN
                p1 = dd(K)
                p2 = K
              ENDIF
              OK     = 1
              IED    = K
              ECUT(1,I)= K
              EXIT
            ENDIF    !  IF (ICUT == 1) THEN
          ENDIF
        ENDDO      !  DO K=1,4
C---
        IF (OK == 1) THEN    ! edge found
          BETA = CRKEDGE(ILAY)%RATIO(IEDGE)
          XIN(1,I) = XXL(p1,I) + BETA*(XXL(p2,I) - XXL(p1,I))
          YIN(1,I) = YYL(p1,I) + BETA*(YYL(p2,I) - YYL(p1,I))
c
          ELFISS(I)   = CRKEDGE(ILAY)%EDGEICRK(IEDGE)    ! Id fissure qui avance
          IENR0(p1,I) = CRKEDGE(ILAY)%EDGEENR(1,IEDGE)   ! enrichissement 1 noeud
          IENR0(p2,I) = CRKEDGE(ILAY)%EDGEENR(2,IEDGE)   ! enrichissement 2 noeud
          EDGEL(IED,I) = 1              ! local : premier edge coupe
          IEDGE  = XEDGE4N(IED,ELCRK)    ! N  edge element sys xfem
          TIP(I) = CRKEDGE(ILAY)%EDGETIP(1,IEDGE)      ! 1 ou 2 , debut ou fin de fissure
        ELSE
          WRITE(IOUT,*) 'ERROR IN ADVANCING CRACK --- CHECK CRACK TIP'
          CALL ARRET(2)
        ENDIF
C---
      END DO !  DO IR=1,NEWCRK
C--------------------------------------------------
c     Search for second intersection (new cut edge)
C--------------------------------------------------
      DO IR=1,NEWCRK
        I = JCT(IR)
        ELCRK = IEL_CRK(I+NFT)
        R     = ECUT(1,I)
        XINT0 = XIN(1,I)
        YINT0 = YIN(1,I)
        DIR11 =-DIR2(ILAY,I)
        DIR22 = DIR1(ILAY,I)
C---
        IF (DIR11 == ZERO) THEN
          DO K=1,3
            R = KPERM(ECUT(1,I) + K)
            IEDGE = XEDGE4N(r,ELCRK)
            NOD1  = NODEDGE(1,IEDGE)
            NOD2  = NODEDGE(2,IEDGE)
            IF (NOD1 == IXC(r+1,I) .and. NOD2 == IXC(dd(r)+1,I))THEN
              p1 = r
              p2 = dd(r)
            ELSE IF (NOD2 == IXC(r+1,I).and.NOD1 == IXC(dd(r)+1,I))THEN
              p1 = dd(r)
              p2 = r
            ENDIF
            DLX = XXL(p2,I) - XXL(p1,I)
            IF (DLX /= ZERO) THEN
              DLY = YYL(p2,I) - YYL(p1,I)
              M12 = DLY / DLX
              XINT = XINT0
              YINT = YYL(p1,I) + M12*(XINT-XXL(p1,I))
              IF ((XINT-XXL(p1,I))*(XINT-XXL(p2,I)) <= ZERO .and.
     .            (YINT-YYL(p1,I))*(YINT-YYL(p2,I)) <= ZERO) THEN
                CROSS = (XXL(p1,I) - XINT)**2 + (YYL(p1,I) - YINT)**2
                BETA  = SQRT(CROSS / LEN(r,I))
                IF (BETA > BMAX .OR. BETA < BMIN) THEN
                  BETA = MAX(BETA, BMIN)
                  BETA = MIN(BETA, BMAX)
                  YINT = YYL(p1,I) + BETA*(YYL(p2,I)-YYL(p1,I))
                ENDIF
C
                ECUT(2,I) = R
                XIN(2,I)  = XINT
                YIN(2,I)  = YINT
                EDGEL(R,I) = 2
                BETA0(R,I) = BETA
                EXIT
              ENDIF
            ENDIF
          ENDDO
c
        ELSEIF (DIR22 == ZERO) THEN
          DO K=1,3
            R = KPERM(ECUT(1,I) + K)
            IEDGE = XEDGE4N(r,ELCRK)
            NOD1  = NODEDGE(1,IEDGE)
            NOD2  = NODEDGE(2,IEDGE)
            IF (NOD1 == IXC(r+1,I) .and. NOD2 == IXC(dd(r)+1,I)) THEN
              p1 = r
              p2 = dd(r)
            ELSE IF (NOD2 == IXC(r+1,I).and.NOD1 == IXC(dd(r)+1,I)) THEN
              p1 = dd(r)
              p2 = r
            ENDIF
            DLY = YYL(p2,I) - YYL(p1,I)
            IF (DLY /= ZERO) THEN
              DLX = XXL(p2,I) - XXL(p1,I)
              M12 = DLX / DLY
              YINT = YINT0
              XINT = XXL(p1,I) + M12*(YINT-YYL(p1,I))
              IF ((XINT-XXL(p1,I))*(XINT-XXL(p2,I)) <= ZERO .and.
     .            (YINT-YYL(p1,I))*(YINT-YYL(p2,I)) <= ZERO) THEN
                CROSS = (XXL(p1,I) - XINT)**2 + (YYL(p1,I) - YINT)**2
                BETA  = SQRT(CROSS / LEN(r,I))
                IF (BETA > BMAX .OR. BETA < BMIN) THEN
                  BETA = MAX(BETA, BMIN)
                  BETA = MIN(BETA, BMAX)
                  XINT = XXL(p1,I) + BETA*(XXL(p2,I)-XXL(p1,I))
                ENDIF
C
                ECUT(2,I)  = R
                XIN(2,I)   = XINT
                YIN(2,I)   = YINT
                EDGEL(r,I) = 2
                BETA0(r,I) = BETA
                EXIT
              ENDIF
            ENDIF
          ENDDO
c
        ELSEIF (DIR11 /= ZERO .and. DIR22 /= ZERO) THEN
          DO K=1,3
            R = KPERM(ECUT(1,I) + K)
            IEDGE = XEDGE4N(r,ELCRK)
            NOD1  = NODEDGE(1,IEDGE)
            NOD2  = NODEDGE(2,IEDGE)
            IF (NOD1 == IXC(r+1,I) .and. NOD2 == IXC(dd(r)+1,I)) THEN
              p1 = r
              p2 = dd(r)
            ELSE IF (NOD2 == IXC(r+1,I).and.NOD1 == IXC(dd(r)+1,I)) THEN
              p1 = dd(r)
              p2 = r
            ENDIF
C
            DLX = XXL(p2,I) - XXL(p1,I)
            DLY = YYL(p2,I) - YYL(p1,I)
            MM = DIR22/DIR11
            IF (DLX == ZERO) THEN
              XINT = XXL(p1,I)
              YINT = YINT0 + MM*(XINT-XINT0)
              IF ((YINT-YYL(p1,I))*(YINT-YYL(p2,I)) <= ZERO) THEN
                CROSS = (YYL(p1,I) - YINT)**2
                BETA  = SQRT(CROSS / LEN(r,I))
                IF (BETA > BMAX .OR. BETA < BMIN) THEN
                  BETA = MAX(BETA, BMIN)
                  BETA = MIN(BETA, BMAX)
                  YINT = YYL(p1,I) + BETA*(YYL(p2,I)-YYL(p1,I))
                ENDIF
                ECUT(2,I)  = R
                XIN(2,I)   = XINT
                YIN(2,I)   = YINT
                EDGEL(r,I) = 2
                BETA0(r,I) = BETA
                EXIT
              ENDIF
            ELSEIF (DLY == ZERO) THEN
              YINT = YYL(p1,I)
              XINT = XINT0 + (YINT0-YYL(p1,I)) / MM
              IF ((XINT-XXL(p1,I))*(XINT-XXL(p2,I)) <= ZERO) THEN
                CROSS = (XXL(p1,I) - XINT)**2
                BETA  = SQRT(CROSS / LEN(r,I))
                IF (BETA > BMAX .OR. BETA < BMIN) THEN
                  BETA = MAX(BETA, BMIN)
                  BETA = MIN(BETA, BMAX)
                  XINT = XXL(p1,I) + BETA*(XXL(p2,I)-XXL(p1,I))
                ENDIF
                ECUT(2,I)  = R
                XIN(2,I)   = XINT
                YIN(2,I)   = YINT
                EDGEL(r,I) = 2
                BETA0(r,I) = BETA
                EXIT
              ENDIF
            ELSE
              M12 = DLY / DLX
              IF (MM /= M12) THEN
                XINT = (YINT0-YYL(p1,I) + M12*XXL(p1,I) - MM*XINT0)/(M12-MM)
                YINT = YINT0 + MM*(XINT-XINT0)
                ACD = (YINT-YYL(p1,I))*(XINT0 - XXL(p1,I))
     .              - (XINT-XXL(p1,I))*(YINT0 - YYL(p1,I))
                BCD = (YINT-YYL(p2,I))*(XINT0 - XXL(p2,I))
     .              - (XINT-XXL(p2,I))*(YINT0 - YYL(p2,I))
                IF (ACD*BCD <= ZERO) THEN
                  CROSS = (XXL(p1,I) - XINT)**2 + (YYL(p1,I) - YINT)**2
                  BETA  = SQRT(CROSS / LEN(r,I))
                  IF (BETA > BMAX .OR. BETA < BMIN) THEN
                    BETA = MAX(BETA, BMIN)
                    BETA = MIN(BETA, BMAX)
                    XINT = XXL(p1,I) + BETA*(XXL(p2,I)-XXL(p1,I))
                    YINT = YYL(p1,I) + BETA*(YYL(p2,I)-YYL(p1,I))
                  ENDIF
                  ECUT(2,I)  = R
                  XIN(2,I)   = XINT
                  YIN(2,I)   = YINT
                  EDGEL(r,I) = 2
                  BETA0(r,I) = BETA
                  EXIT
                ENDIF
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C-----------------------------------------------------------------------
C     check for getting both intersections
C-----------------------------------------------------------------------
      DO IR=1,NEWCRK
        I=JCT(IR)
        FAC = 0
        DO r=1,4
           IF (EDGEL(r,I)==1 .or. EDGEL(r,I)==2) FAC=FAC+1
        ENDDO
        IF (FAC /= 2) THEN
          WRITE(IOUT,*) 'ERROR IN ADVANCING CRACK. NO CUT EDGES'
          CALL ARRET(2)
        ENDIF
      ENDDO
c     tag cut edge numbers of each layer
      DO IR=1,NEWCRK
        I = JCT(IR)
        ELCRK = IEL_CRK(I+NFT)
        DO K=1,4
          IED = EDGEL(K,I)
          CRKEDGE(ILAY)%IEDGEC(K,ELCRK) = IED   ! IED = 0,1,2
        ENDDO
      ENDDO
C----------------------------------------------------------------------
C     SIGN DISTANCE FOR NEW CRACKED LAYER
C----------------------------------------------------------------------
      DO IR=1,NEWCRK
        I = JCT(IR)
        FIT(1,I) = ZERO
        FIT(2,I) = ZERO
        FIT(3,I) = ZERO
        FIT(4,I) = ZERO
c
        DO K=1,4
          p1  = K
          p2  = dd(K)
          IED = EDGEL(K,I)
          IF (IED > 0) THEN
            XMI(IED) = HALF*(XXL(p1,I)+XXL(p2,I))
            YMI(IED) = HALF*(YYL(p1,I)+YYL(p2,I))
          ENDIF
        ENDDO
        DO K=1,4
          FI = ZERO
          CALL LSINT4(XMI(1),YMI(1),XMI(2),YMI(2),XXL(K,I),YYL(K,I),FI)
          IF (FIT(K,I) == ZERO) FIT(K,I)=FI
        ENDDO
      ENDDO
C-------------------
c     Loop over newly cut elements
C-------------------
      DO IR=1,NEWCRK
        I = JCT(IR)
        ELCRK = IEL_CRK(I+NFT)
C
        K = ECUT(2,I)    ! second intersection (tip)
        IEDGE = XEDGE4N(K,ELCRK)
        ICUT  = CRKEDGE(ILAY)%ICUTEDGE(IEDGE)
        IF (ICUT > 0) THEN   ! already cut before => edge connecting two cracks
          CRKEDGE(ILAY)%ICUTEDGE(IEDGE) = 3   ! 2 cracks sur le meme edge
        ELSE
          CRKEDGE(ILAY)%ICUTEDGE(IEDGE) = 2  ! edge cut
          CRKEDGE(ILAY)%RATIO(IEDGE) = BETA0(K,I)
        ENDIF
      ENDDO       !  IR=1,NEWCRK
C-----------------------
C     FILL new cut layer - main loop
C-----------------------
      DO IR=1,NEWCRK
        I = JCT(IR)
        ELCRK = IEL_CRK(I+NFT)        ! n  elem sys xfem
        ELCUTC(1,I) = 2               ! flag de l'element std coupe  (= 1,2)
        NUMELCRK = NUMELCRK + 1       ! + 1 element coupe (pour debug/anim par element)
C
        IADC(1) = IADC_CRK(1,ELCRK)
        IADC(2) = IADC_CRK(2,ELCRK)
        IADC(3) = IADC_CRK(3,ELCRK)
        IADC(4) = IADC_CRK(4,ELCRK)
c
c        if(ispmd==6.and.elcrk==1)    print*,'   ELCRK:,IAD=',ELCRK,IADC(1),IADC(2),IADC(3),IADC(4)
c        if(ispmd==3.and.elcrk==1220) print*,'   ELCRK:,IAD=',ELCRK,IADC(1),IADC(2),IADC(3),IADC(4)
c        if(ispmd==1.and.elcrk==595)  print*,'   ELCRK:,IAD=',ELCRK,IADC(1),IADC(2),IADC(3),IADC(4)
c        if(ispmd==1.and.elcrk==636)  print*,'   ELCRK:,IAD=',ELCRK,IADC(1),IADC(2),IADC(3),IADC(4)
c        if(nspmd==1.and.(elcrk==4059.or.elcrk==3959.or.elcrk==3960.or.elcrk==3860))
c
c
        N(1) = IXC(2,I)               ! n  node sys std
        N(2) = IXC(3,I)
        N(3) = IXC(4,I)
        N(4) = IXC(5,I)
C
        NN(1) = INOD_CRK(N(1))        ! n  node sys xfem
        NN(2) = INOD_CRK(N(2))
        NN(3) = INOD_CRK(N(3))
        NN(4) = INOD_CRK(N(4))
C
        ICRK = ELFISS(I)              ! Id fissure qui avance
C
        ISIGN(1) = INT(SIGN(ONE,FIT(1,I)))
        ISIGN(2) = INT(SIGN(ONE,FIT(2,I)))
        ISIGN(3) = INT(SIGN(ONE,FIT(3,I)))
        ISIGN(4) = INT(SIGN(ONE,FIT(4,I)))
C
        IF (FIT(1,I) == ZERO) ISIGN(1) = 0
        IF (FIT(2,I) == ZERO) ISIGN(2) = 0
        IF (FIT(3,I) == ZERO) ISIGN(3) = 0
        IF (FIT(4,I) == ZERO) ISIGN(4) = 0
c--------------------------------------------
c       activate enriched nodes
c--------------------------------------------
        DO K=1,4
          IF (IENR0(K,I) == 0) THEN
            IENR(K) = CRKNODIAD(IADC(K)) + KNOD2ELC(NN(K))*(ILAY-1) ! default => node std
          ELSE
            IENR(K) = IENR0(K,I)  ! enriched node
          ENDIF
        ENDDO
C
        SIGBETA = 0
        K = ECUT(2,I)  ! only new cut edge (tip)
        IEDGE = XEDGE4N(K,ELCRK)
        NOD1  = NODEDGE(1,IEDGE)
        NOD2  = NODEDGE(2,IEDGE)
        IE10  = CRKEDGE(ILAY)%EDGEENR(1,IEDGE)
        IE20  = CRKEDGE(ILAY)%EDGEENR(2,IEDGE)
        IF (NOD1 == IXC(K+1,I) .and. NOD2 == IXC(dd(K)+1,I)) THEN
          IE1 = IENR(K)
          IE2 = IENR(dd(K))
          IFI1 = ISIGN(K)
          IFI2 = ISIGN(dd(K))
          SIGBETA = IEDGE
        ELSE IF (NOD2 == IXC(K+1,I) .and. NOD1 == IXC(dd(K)+1,I)) THEN
          IE1 = IENR(dd(K))
          IE2 = IENR(K)
          IFI1 = ISIGN(dd(K))
          IFI2 = ISIGN(K)
          SIGBETA = -IEDGE
        END IF
        CRKEDGE(ILAY)%EDGEENR(1,IEDGE) = MAX(IE1,IE10)
        CRKEDGE(ILAY)%EDGEENR(2,IEDGE) = MAX(IE2,IE20)
        IF (CRKEDGE(ILAY)%EDGEICRK(IEDGE) == 0)
     .      CRKEDGE(ILAY)%EDGEICRK(IEDGE)  = ICRK
c--------------------------
        ITRI = 0
        NM   = 0
        NP   = 0
        DO K=1,4
          IED = EDGEL(K,I)
          IF (IED > 0) THEN
            IEDGE = XEDGE4N(K,ELCRK)
            CRKEDGE(ILAY)%EDGETIP(1,IEDGE) = TIP(I)
            CRKEDGE(ILAY)%EDGETIP(2,IEDGE) =
     .                  CRKEDGE(ILAY)%EDGETIP(2,IEDGE) + 1
          ENDIF
          IF (ISIGN(K) > 0) THEN
            ITRI = ITRI + 1
            NP   = K
          ELSEIF (ISIGN(K) < 0) THEN
            NM = K
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
          IF (NP > 1 .and. ISIGN(NP-1) > 0)  THEN
            NX1 = NP-1
          ELSE
            NX1 = NP
          ENDIF
        ENDIF
c
c          print*,'   Avancement: IEL,ELCRK,proc=',I+NFT,ELCRK,ispmd
c          print*,'               IAD=',IADC(1),IADC(2),IADC(3),IADC(4)
c          print*,'               NSX=',NN(1),NN(2),NN(3),NN(4),ITRI
c---
        NX2 = KPERM(NX1+1)
        NX3 = KPERM(NX1+2)
        NX4 = KPERM(NX1+3)
C
        CRKEDGE(ILAY)%LAYCUT(ELCRK) = 1
        XFEM_PHANTOM(ILAY)%ITRI(1,ELCRK) = ITRI
        XFEM_PHANTOM(ILAY)%ITRI(2,ELCRK) = NX1
        XFEM_PHANTOM(ILAY)%ELCUT(ELCRK)  = ICRK
        XFEM_PHANTOM(ILAY)%IFI(IADC(1))  = ISIGN(1)
        XFEM_PHANTOM(ILAY)%IFI(IADC(2))  = ISIGN(2)
        XFEM_PHANTOM(ILAY)%IFI(IADC(3))  = ISIGN(3)
        XFEM_PHANTOM(ILAY)%IFI(IADC(4))  = ISIGN(4)
C------------------
C       IXEL = 1 => positif element within ILAY  (ILEV = PP1)
C------------------
        CRKLVSET(PP1)%ENR0(1,IADC(1)) = -IENR(1)
        CRKLVSET(PP1)%ENR0(1,IADC(2)) = -IENR(2)
        CRKLVSET(PP1)%ENR0(1,IADC(3)) = -IENR(3)
        CRKLVSET(PP1)%ENR0(1,IADC(4)) = -IENR(4)
C
        IF (ISIGN(1) > 0) CRKLVSET(PP1)%ENR0(1,IADC(1)) = 0
        IF (ISIGN(2) > 0) CRKLVSET(PP1)%ENR0(1,IADC(2)) = 0
        IF (ISIGN(3) > 0) CRKLVSET(PP1)%ENR0(1,IADC(3)) = 0
        IF (ISIGN(4) > 0) CRKLVSET(PP1)%ENR0(1,IADC(4)) = 0
C------------------
C       IXEL = 2 => negatif element within ILAY (ILEV = PP2)
C------------------
        CRKLVSET(PP2)%ENR0(1,IADC(1)) = -IENR(1)
        CRKLVSET(PP2)%ENR0(1,IADC(2)) = -IENR(2)
        CRKLVSET(PP2)%ENR0(1,IADC(3)) = -IENR(3)
        CRKLVSET(PP2)%ENR0(1,IADC(4)) = -IENR(4)
C
        IF (ISIGN(1) < 0) CRKLVSET(PP2)%ENR0(1,IADC(1)) = 0
        IF (ISIGN(2) < 0) CRKLVSET(PP2)%ENR0(1,IADC(2)) = 0
        IF (ISIGN(3) < 0) CRKLVSET(PP2)%ENR0(1,IADC(3)) = 0
        IF (ISIGN(4) < 0) CRKLVSET(PP2)%ENR0(1,IADC(4)) = 0
C------------------
C       IXEL = 3 => Third phantom element  (ILEV = PP3)
C------------------
        IF (ITRI == 0) THEN
          X1  = XXL(NX1,I)
          Y1  = YYL(NX1,I)
          X2  = XXL(NX2,I)
          Y2  = YYL(NX2,I)
          IED = EDGEL(NX2,I)
          IF (IED > 0) THEN
            X3 = XIN(IED,I)
            Y3 = YIN(IED,I)
          ELSE
          ENDIF
          IED = EDGEL(NX4,I)
          IF (IED > 0) THEN
            X4 = XIN(IED,I)
            Y4 = YIN(IED,I)
          ELSE
          ENDIF
          AREA1 = HALF*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))
     .
          AREA1 = AREA1 * A_I(I)
          AREA2 = ONE - AREA1
          AREA3 = ZERO
c
          XFEM_STR(3)%GBUF%OFF(I) = ZERO
c          XFEM_STR(3)%BUFLY(ILAY)%OFF(I) = 0
          XFEM_STR(3)%BUFLY(ILAY)%LBUF(1,1,1)%OFF(I) = ZERO
c
        ELSEIF (ITRI < 0) THEN   ! cut en 3,  IPH3 = IPH2 < 0
            IE1 = XEDGE4N(NX2,ELCRK)
            IE2 = XEDGE4N(NX4,ELCRK)
c
            IF (CRKEDGE(ILAY)%ICUTEDGE(IE2) > 1) THEN
              SIGBETA = -SIGBETA
              IAD1 = IADC(NX1)
              IAD2 = IADC(NX2)
              IAD3 = IADC(NX3)
              IAD4 = IADC(NX4)
              NOD1 = IAD4
              NOD2 = IAD1
              CRKLVSET(PP3)%ENR0(1,IAD1) = ABS(CRKLVSET(PP2)%ENR0(1,IAD1))
              CRKLVSET(PP3)%ENR0(1,IAD2) = CRKLVSET(PP2)%ENR0(1,IAD2)
              CRKLVSET(PP3)%ENR0(1,IAD3) = CRKLVSET(PP2)%ENR0(1,IAD3)
              CRKLVSET(PP3)%ENR0(1,IAD4) = CRKLVSET(PP2)%ENR0(1,IAD4)
              CRKLVSET(PP2)%ENR0(1,IAD1) = -CRKNODIAD(IAD1) - KNOD2ELC(NN(NX1))*(ILAY-1)
c
c--           IXEL=2 AREA factor
              IED = CRKEDGE(ILAY)%IEDGEC(NX4,ELCRK)
              X1  = XIN(IED,I)
              Y1  = YIN(IED,I)
              X2  = XXL(NX3,I)
              Y2  = YYL(NX3,I)
              X3  = XXL(NX4,I)
              Y3  = YYL(NX4,I)
              AREA2 = HALF*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))
c--           IXEL=1 AREA factor
              IED = CRKEDGE(ILAY)%IEDGEC(NX1,ELCRK)
              X2  = XIN(IED,I)
              Y2  = YIN(IED,I)
              X3  = XXL(NX1,I)
              Y3  = YYL(NX1,I)
              AREA1 = HALF*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))
c
              AREA1 = AREA1 * A_I(I)
              AREA2 = AREA2 * A_I(I)
              AREA3 = ONE - AREA1 - AREA2
            ELSEIF (CRKEDGE(ILAY)%ICUTEDGE(IE1) > 1) THEN
               print*,' IMPOSSIBLE CASE'
            ENDIF
C
          ELSEIF (ITRI > 0) THEN   !  cut en 3,  IPH3 = IPH1
            IE1 = XEDGE4N(NX1,ELCRK)
            IE2 = XEDGE4N(NX4,ELCRK)
c
            IF (CRKEDGE(ILAY)%ICUTEDGE(IE1) > 1) THEN
              IAD1 = IADC(NX1)
              IAD2 = IADC(NX2)
              IAD3 = IADC(NX3)
              IAD4 = IADC(NX4)
              NOD1 = IAD2
              NOD2 = IAD1
              CRKLVSET(PP3)%ENR0(1,IAD1) = ABS(CRKLVSET(PP1)%ENR0(1,IAD1))
              CRKLVSET(PP3)%ENR0(1,IAD2) = CRKLVSET(PP1)%ENR0(1,IAD2)
              CRKLVSET(PP3)%ENR0(1,IAD3) = CRKLVSET(PP1)%ENR0(1,IAD3)
              CRKLVSET(PP3)%ENR0(1,IAD4) = CRKLVSET(PP1)%ENR0(1,IAD4)
              CRKLVSET(PP1)%ENR0(1,IAD1) = -CRKNODIAD(IAD1) - KNOD2ELC(NN(NX1))*(ILAY-1)
c--           IXEL=1 AREA factor
              IED = CRKEDGE(ILAY)%IEDGEC(NX1,ELCRK)
              X1  = XIN(IED,I)
              Y1  = YIN(IED,I)
              X2  = XXL(NX2,I)
              Y2  = YYL(NX2,I)
              X3  = XXL(NX3,I)
              Y3  = YYL(NX3,I)
              AREA1 = HALF*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))
c--           IXEL=2 AREA factor
              IED = CRKEDGE(ILAY)%IEDGEC(NX4,ELCRK)
              X2  = XIN(IED,I)
              Y2  = YIN(IED,I)
              X3  = XXL(NX1,I)
              Y3  = YYL(NX1,I)
              AREA2 = HALF*ABS((X1-X3)*(Y2-Y1) - (X1-X2)*(Y3-Y1))
c
              AREA1 = AREA1 * A_I(I)
              AREA2 = AREA2 * A_I(I)
              AREA3 = ONE - AREA1 - AREA2
            ELSEIF (CRKEDGE(ILAY)%ICUTEDGE(IE2) > 1) THEN
               print*,' IMPOSSIBLE CASE'
            ENDIF
          ENDIF  ! ITRI
c----
          CRKLVSET(PP1)%AREA(ELCRK) = AREA1
          CRKLVSET(PP2)%AREA(ELCRK) = AREA2
          CRKLVSET(PP3)%AREA(ELCRK) = AREA3
c
          IF (AREA3 < ZERO .or. AREA1 > ONE .or. AREA2 > ONE .or. AREA3 > ONE ) THEN
            print*,'ERROR : XFEM PHANTOM ELEMENT AREA: ELCRK=',ELCRK
          ENDIF
c
      ENDDO ! IR=1,NEWCRK
c-----------
      RETURN
      END
Chd|====================================================================
Chd|  LSINT4                        source/elements/xfem/crklayer4n_adv.F
Chd|-- called by -----------
Chd|        CRKLAYER3N_ADV                source/elements/xfem/crklayer3n_adv.F
Chd|        CRKLAYER3N_INI                source/elements/xfem/crklayer3n_ini.F
Chd|        CRKLAYER4N_ADV                source/elements/xfem/crklayer4n_adv.F
Chd|        CRKLAYER4N_INI                source/elements/xfem/crklayer4n_ini.F
Chd|        ENRICHC_INI                   source/elements/xfem/enrichc_ini.F
Chd|        ENRICHTG_INI                  source/elements/xfem/enrichtg_ini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LSINT4(Y1, Z1, Y2, Z2, Y, Z, FI)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      MY_REAL
     .   Y1,Z1,Y2,Z2,Y,Z,FI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      MY_REAL
     . AREA,AB
C-----------------------------------------------
      FI = ZERO
      AREA = ((Y2*Z-Y*Z2)-(Y1*Z-Y*Z1)+(Y1*Z2-Z1*Y2))
      AB   = (Y2-Y1)**2 + (Z2-Z1)**2
      IF (AB > ZERO) FI = AREA/SQRT(AB)
C-----------
      RETURN
      END
