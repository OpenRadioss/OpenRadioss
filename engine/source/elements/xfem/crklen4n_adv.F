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
Chd|  CRKLEN4N_ADV                  source/elements/xfem/crklen4n_adv.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE CRKLEN4N_ADV(
     .           NEL       ,NFT       ,ILAY      ,NLAY      ,IXC       ,
     .           CRKLEN    ,ELCRKINI  ,IEL_CRK   ,DIR1      ,DIR2      ,     
     .           NODEDGE   ,CRKEDGE   ,XEDGE4N   ,NGL       ,XL2       ,
     .           XL3       ,XL4       ,YL2       ,YL3       ,YL4       ,
     .           ALDT      )
C-----------------------------------------------
C   crack advancement, shells 4N
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
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NFT,ILAY,NLAY
      INTEGER IXC(NIXC,*),NGL(NEL),IEL_CRK(*),ELCRKINI(NLAY,NEL),
     .   NODEDGE(2,*),XEDGE4N(4,*)
      my_real DIR1(NLAY,NEL),DIR2(NLAY,NEL),CRKLEN(NEL),ALDT(NEL),
     .  XL2(NEL),YL2(NEL),XL3(NEL),YL3(NEL),XL4(NEL),YL4(NEL)
      TYPE (XFEM_EDGE_)   , DIMENSION(*)    :: CRKEDGE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,R,IR,P1,P2,NEWCRK,IED,OK,ELCRK,NX1,NX2,NX3,NX4,NM,NP,
     . FAC,IFI1,IFI2,IEDGE,ICUT,SIGBETA,ICRK,IELCRK,NOD1,NOD2
c
      INTEGER JCT(NEL),EDGEL(4,NEL),TIP(NEL),ECUT(2,NEL),dd(4),d(8),KPERM(8)
c
      my_real
     .   XIN(2,NEL),YIN(2,NEL),LEN(4,NEL),XMI(2),YMI(2),
     .   XXL(4,NEL),YYL(4,NEL),BETA0(4,NEL)
      my_real
     .   XINT,YINT,ZINT,FI,XXX,YYY,ZZZ,CROSS,ACD,BCD,DLX,DLY,
     .   X10,Y10,Z10,X20,Y20,Z20,D12,M12,MM,XINT0,YINT0,DIR11,DIR22,
     .   X1,Y1,X2,Y2,X3,Y3,X4,Y4,BETA,BMIN,BMAX
C----------
      DATA d/1,2,2,3,4,3,1,4/
      DATA dd/2,3,4,1/
      DATA KPERM/1,2,3,4,1,2,3,4/
      PARAMETER (BMIN = 0.01, BMAX = 0.99)
C=======================================================================
      NEWCRK = 0
      DO I=1,NEL
        JCT(I) = 0
        IF (ELCRKINI(ILAY,I) == 5) THEN        ! avancement de fissure
          NEWCRK = NEWCRK + 1
          JCT(NEWCRK) = I
          ELCRKINI(ILAY,I) = 2                 ! reset pour l avancement
        ELSEIF (ELCRKINI(ILAY,I) == -5) THEN   ! initialisation de fissure
          CRKLEN(I) = ALDT(I)
          ELCRKINI(ILAY,I) = 0                 ! reset pour initialisation
        ENDIF
      ENDDO
      IF (NEWCRK == 0) RETURN
C---
      DO IR=1,NEWCRK 
        I = JCT(IR)              
        TIP(I)   = 0
        ECUT(1:2,I)  = 0
        EDGEL(1:4,I) = 0
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
        I = JCT(IR)
        FAC = 0
        DO r=1,4
           IF (EDGEL(r,I)==1 .or. EDGEL(r,I)==2) FAC=FAC+1
        ENDDO
        IF (FAC /= 2) THEN 
          WRITE(IOUT,*) 'ERROR IN ADVANCING CRACK. NO CUT EDGES'
          CALL ARRET(2)           
        ENDIF
        CRKLEN(I) = SQRT((XIN(2,I) - XIN(1,I))**2 + (YIN(2,I) - YIN(1,I))**2)
      ENDDO
c-----------
      RETURN
      END
